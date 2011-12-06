goog.require('goog.net.XhrIo');
goog.require('goog.dom');
goog.require('goog.events');
goog.require('goog.array');
goog.require('goog.style');

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

function copyTo(valueTo) {
  return function(valueFrom) {
    valueTo.set(valueFrom.get());
  };
};

hwt.PollingHandler = function() {
  this.values = {};
};
hwt.PollingHandler.prototype.poll = function() {
  var that = this;
  this.handleUpdates(function(updates) {
    for (persistentName in updates) {
      that.values[persistentName].init(updates[persistentName]);
    };
    window.setTimeout(function() {that.poll()},pollingInterval);
  });
}
hwt.PollingHandler.prototype.handleUpdates = function(f) {
  goog.net.XhrIo.send('updates', function(e) {
    var xhr = e.target;
    var r = xhr.getResponseJson();
    //alert(r);
    if (r.OK) {
      f(r.updates);
    } else {
      alert('Updating Values failed!');
    }
  });
};
hwt.PollingHandler.prototype.addValue = function(name,value) {
  this.values[name] = value;
};
hwt.PollingHandler.prototype.get = function(persistentName,f) {
  goog.net.XhrIo.send('updates/' + persistentName, function(e) {
    var xhr = e.target;
    var r = xhr.getResponseJson();
    //alert(r);
    if (r.OK) {
      f(r.value);
    } else {
      alert('Updating Value ' + persistentName + ' failed!');
    }
  });
};

hwt.TransientValue = function(pollingHandler,content,transientName) {
  this.content = content;
  this.models = new Array();
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
  var that = this;
  goog.net.XhrIo.send('value/' + that.persistentName, function(e) {
    var xhr = e.target;
    var t = xhr.getResponseText();
    //alert(t);
  },'POST',content);
}

hwt.ServerValue = function(pollingHandler,persistentName) {
  hwt.TransientValue.call(this,pollingHandler,null,persistentName);
  var that = this;
  pollingHandler.get(persistentName,function(content) {
    that.init(content);
  });
};
goog.inherits(hwt.ServerValue,hwt.TransientValue);
hwt.ServerValue.prototype.set = function(){alert('Setting ServerValues is not permitted!');};

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

hwt.ReadModel = function(value) {
  hwt.Model.call(this);
  value.models.push(this);
  this.value = value;
};
goog.inherits(hwt.ReadModel,hwt.Model);
hwt.ReadModel.prototype.get = function() {return this.value.get()};

hwt.ReadWriteModel = function(value) {
  hwt.ReadModel.call(this,value);
};
goog.inherits(hwt.ReadWriteModel,hwt.ReadModel);
hwt.ReadWriteModel.prototype.set = function(content) {
  this.value.set(content);
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
}
goog.inherits(hwt.NegateModel,hwt.Model);

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

hwt.ContainerWidget = function(getBodyNode,widgets) {
  this.subWidgets = widgets;
  this.subWidgets.forEach(function(subWidget) {
    goog.dom.appendChild(getBodyNode(),subWidget.domNode);
  });
}

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

hwt.widgets.Button = function(textModel,action,actionModel,opt_disabledModel) {
  hwt.Widget.call(this,goog.dom.createDom('button'));
  hwt.DisableableWidget.call(this,this.getRootNode,opt_disabledModel);
  hwt.TextContentWidget.call(this,this.getRootNode,textModel);
  goog.events.listen(this.domNode,goog.events.EventType.CLICK,function(){
    action(actionModel);
  });
}
goog.inherits(hwt.widgets.Button,hwt.Widget);

hwt.widgets.Panel = function(opt_visibleModel,opt_classModel,var_subWidgets) {
  hwt.Widget.call(this,goog.dom.createDom('div'));
  hwt.ClassWidget.call(this,this.getRootNode,opt_classModel);
  hwt.HideableWidget.call(this,this.getRootNode,opt_visibleModel);
  hwt.ContainerWidget.call(this,this.getRootNode,var_subWidgets);
};
goog.inherits(hwt.widgets.Panel,hwt.Widget);
