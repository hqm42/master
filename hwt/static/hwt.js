goog.require('goog.net.XhrIo');
goog.require('goog.dom');
goog.require('goog.events');
goog.require('goog.array');
goog.require('goog.style');
goog.require('goog.json');

goog.provide('hwt.TransientValue');
goog.provide('hwt.Value');
goog.provide('hwt.Model');
goog.provide('hwt.ReadModel');
goog.provide('hwt.ReadWriteModel');
goog.provide('hwt.Slot');
goog.provide('hwt.Widget');
goog.provide('hwt.widgets.Label')
goog.provide('hwt.widgets.Textfield');
goog.provide('hwt.widgets.Checkbox');
goog.provide('hwt.widgets.Button');
goog.provide('hwt.widgets.Panel');

pollingInterval = 0;

// dlh

function getAllValues(all,cons,valueMap) {
  all.push(cons);
  if (cons.content.childRefs && cons.content.childRefs.length == 2) {
    getAllValues(all,valueMap[cons.content.childRefs[1]],valueMap);
  };
};

function newValueById(ph,name,content) {
  var sw = function(prefix) {return name.search(prefix) == 0;};
  if (sw("vS")) {
    return new hwt.ServerValue(ph,content,name);
  } else if (sw("vC") || sw("vW")) {
    return new hwt.Value(ph,content,name);
  } else if (sw("vT")) {
    return new hwt.TransientValue(ph,content,name);
  }  else {
    alert("unknown value type: " + name);
    return null;
  }
}

// end dlh

hwt.PollingHandler = function(windowKey) {
  this.windowKey = windowKey;
  this.values = {};
};
hwt.PollingHandler.prototype.poll = function() {
  var that = this;
  this.handleUpdates(function(updates) {
    for (persistentName in updates.new) {
      that.values[persistentName] = newValueById(that,persistentName,updates.new[persistentName]);
    };
    for (persistentName in updates.changed) {
      that.values[persistentName].init(updates.changed[persistentName]);
    };
    for (persistentName in updates.removed) {
      delete that.values[persistentName]
    }
    window.setTimeout(function() {that.poll()},pollingInterval);
  });
}
hwt.PollingHandler.prototype.handleUpdates = function(f) {
  goog.net.XhrIo.send('u?windowKey=' + this.windowKey, function(e) {
    var xhr = e.target;
    var r = xhr.getResponseJson();
    f(r);
  });
};
hwt.PollingHandler.prototype.addValue = function(name,value) {
  this.values[name] = value;
};
hwt.PollingHandler.prototype.get = function(persistentName,f) {
  goog.net.XhrIo.send('u/' + persistentName + "?windowKey=" + this.windowKey, function(e) {
    var xhr = e.target;
    var r = xhr.getResponseJson();
    f(r);
  });
};
hwt.PollingHandler.prototype.set = function(persistentName,content) {
  goog.net.XhrIo.send('v/' + persistentName + "?windowKey=" + this.windowKey, function(e) {
    var xhr = e.target;
    var t = xhr.getResponseText();
    //alert(t);
  },'POST',goog.json.serialize(content));
};
hwt.PollingHandler.prototype.getValue = function(name) {
  return this.values[name];
};

hwt.TransientValue = function(pollingHandler,content,transientName) {
  this.content = content;
  this.models = new Array();
  this.pollingHandler = pollingHandler;
  pollingHandler.addValue(transientName,this);
};
hwt.TransientValue.prototype.get = function() {return this.content};
hwt.TransientValue.prototype.set = function(content) {
  this.content = content;
  this.models.forEach(function(model) {
    model.notify();
  });
};
hwt.TransientValue.prototype.init = hwt.TransientValue.prototype.set;

hwt.Value = function(pollingHandler,content,persistentName) {
  hwt.TransientValue.call(this,pollingHandler,content,persistentName);
  this.persistentName = persistentName;
};
goog.inherits(hwt.Value,hwt.TransientValue);
hwt.Value.prototype.set = function(content) {
  hwt.TransientValue.prototype.set.call(this,content);
  this.pollingHandler.set(this.persistentName,content);
}

hwt.ServerValue = function(pollingHandler,content,persistentName) {
  hwt.Value.call(this,pollingHandler,content,persistentName);
};
goog.inherits(hwt.ServerValue,hwt.Value);
hwt.ServerValue.prototype.set = function(){alert('Setting ServerValues is not permitted!');};

hwt.project = function(valueMap,startValue,steps) {
  var v = startValue;
  for (var i in steps) {
    v = valueMap[v.get().childRefs[steps[i]]];
  }
  return v;
};

hwt.Model = function() {
  this.slots = new Array();
};
hwt.Model.prototype.notify = function() {
  this.slots.forEach(function(slot) {
    slot();
  });
};
hwt.Model.prototype.addSlot = function(slot) {
  this.slots.push(slot);
  slot(); // updates the widget
};

hwt.ModelmR = function(value) {
  hwt.Model.call(this);
  value.models.push(this);
  this.value = value;
};
goog.inherits(hwt.ModelmR,hwt.Model);
hwt.ModelmR.prototype.get = function() {return this.value.get()};

hwt.ModelmRW = function(value) {
  hwt.ModelmR.call(this,value);
};
goog.inherits(hwt.ModelmRW,hwt.ModelmR);
hwt.ModelmRW.prototype.set = function(content) {
  this.value.set(content);
};

hwt.ListModelmR = function(listRootValue) {
  hwt.Model.call(this);
  var all = new Array();
  getAllValues(all,listRootValue,listRootValue.pollingHandler.values);
  var that = this;
  all.forEach(function(value) {value.models.push(that)})
  this.listRoot = listRootValue;
};
goog.inherits(hwt.ListModelmR,hwt.Model);
hwt.ListModelmR.prototype.get = function() {
  var res = new Array();
  var that = this;
  getAllValues(res,this.listRoot,this.listRoot.pollingHandler.values);
  res.forEach(function(value){
    var i = value.models.indexOf(that);
    if (i == -1) value.models.push(that);
  });
  return res;
};

hwt.NegateModel = function(model) {
  var that = this;
  hwt.Model.call(this);
  if (goog.isDef(model.get)) {
    this.get = function() {return ! model.get();};
  };
  if (goog.isDef(model.set)) {
    this.set = function(c) {model.set(!c)};
  };
  model.addSlot(function() {
    that.notify()
  });
};
goog.inherits(hwt.NegateModel,hwt.Model);

hwt.EqualsModel = function(m1,m2) {
  hwt.Model.call(this);
  var that = this;
  this.get = function() {
    return m1.get() == m2.get();
  };
  var slot = function() {
    that.notify();
  };
  m1.addSlot(slot);
  m2.addSlot(slot);
};
goog.inherits(hwt.EqualsModel,hwt.Model);

hwt.Widget = function(domNode) {
  this.domNode = domNode;
  var that = this;
  this.getRootNode = function() {return that.domNode};
};

hwt.DisableableWidget = function(getElementToDisable,disabledModel) {
  var that = this;
  if (disabledModel) {
    disabledModel.addSlot(function() {
      if (disabledModel.get()) {
        goog.dom.setProperties(getElementToDisable(),{'disabled':'disabled'})
      } else {
        getElementToDisable().removeAttribute('disabled');
      }
    });
  }
};

hwt.TextContentWidget = function(getTextParent,textModel) {
  var that = this;
  if (textModel) {
    textModel.addSlot(function() {
      goog.dom.setTextContent(getTextParent(),textModel.get());
    });
  }
};

hwt.ClassWidget = function(getClassParent,classModel) {
  var that = this;
  if (classModel) {
    classModel.addSlot(function() {
      getClassParent().className = classModel.get();
    });
  };
};

hwt.HideableWidget = function(getHiddenParent,visibleModel) {
  var that = this;
  if (visibleModel) {
    visibleModel.addSlot(function() {
      goog.style.showElement(getHiddenParent(), visibleModel.get())
    })
  };
};

hwt.ContainerWidget = function(getBodyNode,getSubWidgets) {
  goog.dom.removeChildren(getBodyNode());
  this.subWidgets = getSubWidgets();
  this.subWidgets.forEach(function(subWidget) {
    goog.dom.appendChild(getBodyNode(),subWidget.domNode);
  });
};

hwt.widgets.Label = function(textModel,opt_classModel) {
  hwt.Widget.call(this,goog.dom.createDom('span'));
  hwt.TextContentWidget.call(this,this.getRootNode,textModel);
  hwt.ClassWidget.call(this,this.getRootNode,opt_classModel);
};
goog.inherits(hwt.widgets.Label,hwt.Widget);


hwt.widgets.TextField = function(textModel,opt_disabledModel,opt_classModel) {
  hwt.Widget.call(this,goog.dom.createDom('input',{'type':'text'}));
  hwt.DisableableWidget.call(this,this.getRootNode,opt_disabledModel);
  hwt.ClassWidget.call(this,this.getRootNode,opt_classModel);
  var that = this;
  textModel.addSlot(function() {
    goog.dom.setProperties(that.domNode,{'value':textModel.get()});
  });
  goog.events.listen(this.domNode,goog.events.EventType.INPUT,function() {
    textModel.set(that.domNode.value);
  });
};
goog.inherits(hwt.widgets.TextField,hwt.Widget);

hwt.widgets.Checkbox = function(checkedModel,opt_disabledModel) {
  hwt.Widget.call(this,goog.dom.createDom('input',{'type':'checkbox'}));
  hwt.DisableableWidget.call(this,this.getRootNode,opt_disabledModel);
  var that = this;
  checkedModel.addSlot(function() {
    if (checkedModel.get()) {
      goog.dom.setProperties(that.domNode,{'checked':'checked'});
    } else {
      that.domNode.removeAttribute('checked');
    }
  });
  goog.events.listen(this.domNode,goog.events.EventType.CHANGE,function() {
    checkedModel.set(that.domNode.checked);
  });
};
goog.inherits(hwt.widgets.Checkbox,hwt.Widget);

hwt.widgets.Button = function(textModel,action,opt_disabledModel) {
  hwt.Widget.call(this,goog.dom.createDom('button'));
  hwt.DisableableWidget.call(this,this.getRootNode,opt_disabledModel);
  hwt.TextContentWidget.call(this,this.getRootNode,textModel);
  goog.events.listen(this.domNode,goog.events.EventType.CLICK,function(){
    action();
  });
}
goog.inherits(hwt.widgets.Button,hwt.Widget);

hwt.widgets.Panel = function(opt_visibleModel,opt_classModel,var_subWidgets) {
  hwt.Widget.call(this,goog.dom.createDom('div'));
  hwt.ClassWidget.call(this,this.getRootNode,opt_classModel);
  hwt.HideableWidget.call(this,this.getRootNode,opt_visibleModel);
  hwt.ContainerWidget.call(this,this.getRootNode,function(){return var_subWidgets});
};
goog.inherits(hwt.widgets.Panel,hwt.Widget);

hwt.widgets.List = function(listModel,subWidget) {
  hwt.Widget.call(this,goog.dom.createDom('div'));
  var rebuildSubWidgets = function(){
    var ccs = listModel.get();
    ccs.pop(); // delete last cell []
    var res = new Array();
    ccs.forEach(function(value){
      var elem = value.pollingHandler.values[value.content.childRefs[0]]
      res.push(new subWidget(elem))})
    return res};
  hwt.ContainerWidget.call(this,this.getRootNode,rebuildSubWidgets);
  var that = this;
  listModel.addSlot(function(){
      hwt.ContainerWidget.call(that,that.getRootNode,rebuildSubWidgets);
    });
};
goog.inherits(hwt.widgets.List,hwt.Widget);
