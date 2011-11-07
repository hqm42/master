function mkContainer(/*children*/) {
  var d = document.createElement('div');
  for (var i=0, len=arguments.length; i<len; i++) {
    d.appendChild(arguments[i]);
  }
  return d;
} 

function mkLabel(text) {
  var l = document.createElement('span');
  var t = document.createTextNode(text);
  l.appendChild(t);
  return l;
}

function mkButton(text,action) {
  var b = document.createElement('button');
  var t = document.createTextNode(text);
  b.appendChild(t);
  b.setAttribute('onclick', 'actions[\'' + action + '\']()');
  return b;
}

// helper begin

function getModel(elem) {
  return elem.firstChild.textContent
}

function setModel(elem,model) {
  newContent = document.createTextNode(model);
  elem.replaceChild(newContent, elem.firstChild);
}

// helper end

actions = {} // global FIXME

function mkAction(target,actionName) {
   var a = function() {
      var xmlhttp = new XMLHttpRequest();
      xmlhttp.open("POST","action/" + actionName,true);
      xmlhttp.onreadystatechange=function() {
        if (xmlhttp.readyState!=4) {
          setModel(target,xmlhttp.responseText)
        };
        xmlhttp.setRequestHeader('Content-Type','text/plain');
     }
     xmlhttp.send(getModel(target));
  }
  actions[actionName] = a;
  return actionName
}
