remark.macros.scale = function (percentage) {
  var url = this;
  return '<img src="' + url + '" style="width: ' + percentage + '; height: ' + percentage + '" />';
};

remark.macros.scalew = function (percentage) {
  var url = this;
  return '<img src="' + url + '" style="width: ' + percentage + '" />';
};

remark.macros.scaleh = function (percentage) {
  var url = this;
  return '<img src="' + url + '" style="height: ' + percentage + '" />';
};

remark.macros.pdf = function() {
  var url = this;
  return '<object data="' + url + '" type="application/pdf" width="700px" height="700px"> <embed src="' + url + ' "> </embed> </object>';
}



