function mkContainer(/*children*/) {
  var d = document.createElement('div');
  for (var i=0, len=arguments.length; i<len; i++) {
    d.appendChild(arguments[i]);
  }
  return d;
} 

function mkLabel() {
  var l = document.createElement('span');
  textContentModel(l);
  return l;
}

function mkButton(action) {
  var b = document.createElement('button');
  b.hwtAction = action;
  b.setAttribute('onclick', 'this.hwtAction()');
  textContentModel(b);
  return b;
}

function mkTextField(modelName) {
  var t = document.createElement('input');
  t.type = 'text';
  valueModel(t);
  userUpdateableModel(t);
  return t;
}

function mkAction(target,actionName) {
   var a = function() {
      var xmlhttp = new XMLHttpRequest();
      xmlhttp.open("POST","action/" + actionName,true);
      xmlhttp.onreadystatechange=function() {
        if (xmlhttp.readyState!=4) {
          target.hwtSetModel(xmlhttp.responseText)
        };
     }
     xmlhttp.setRequestHeader('Content-Type','text/plain');
     xmlhttp.send(target.hwtGetModel());
  }
  return a;
}

// helper begin

function userUpdateableModel(elem) {
  elem.hwtModelUpdate = function() {
    var xmlhttp = new XMLHttpRequest();
    xmlhttp.open("POST","model/" + elem.name ,true);
    xmlhttp.onreadystatechange=function() {
      if (xmlhttp.readyState!=4) {
        // alert(xmlhttp.responseText);
      };
   }
   xmlhttp.setRequestHeader('Content-Type','text/plain');
   xmlhttp.send(elem.hwtGetModel());
  };
  elem.setAttribute('onkeyup','this.hwtModelUpdate()');
}

function textContentModel(elem) {
  elem.hwtSetModel = function(model){setModel(elem,model)};
  elem.hwtGetModel = function(){getModel(elem)};
}

function valueModel(elem) {
  elem.hwtSetModel = function(model){elem.value = model};
  elem.hwtGetModel = function(){return elem.value};
}

function getModel(elem) {
  return elem.firstChild.textContent
}

function setModel(elem,model) {
  var newContent = document.createTextNode(model);
  if (elem.childNodes && elem.childNodes.length > 0) {
    elem.replaceChild(newContent, elem.firstChild);
  } else {
    elem.appendChild(newContent);
  }
}

// helper end
