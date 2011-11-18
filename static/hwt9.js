goog.require('goog.net.XhrIo');
goog.require('goog.dom');
goog.require('goog.events');
goog.provide('goog.ui');

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

hwt.Value = function(content) {
  this.content = content;
  this.models = new Array();
};
hwt.Value.prototype.get = function() {return this.content};
hwt.Value.prototype.set = function(content) {
  this.content = content;
  this.models.forEach(function(model) {
    model.notify();
  });
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
}

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

hwt.Widget = function(domNode) {
  this.domNode = domNode;
  var that = this;
  this.getRootNode = function() {return that.domNode};
};
//hwt.Widget.prototype.getRootNode = function() {return this.domNode};

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
}

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
  }
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

hwt.widgets.Button = function(textModel,action,actionModel,opt_disabledModel) {
  hwt.Widget.call(this,goog.dom.createDom('button'));
  hwt.DisableableWidget.call(this,this.getRootNode,opt_disabledModel);
  hwt.TextContentWidget.call(this,this.getRootNode,textModel);
  goog.events.listen(this.domNode,goog.events.EventType.CLICK,function(){
    action(actionModel);
  });
}
goog.inherits(hwt.widgets.Button,hwt.Widget);
