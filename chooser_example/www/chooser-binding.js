(function() {
  
  function updateChooser(chooser) {
    chooser = $(chooser);
    var left = chooser.find("select.left");
    var right1 = chooser.find("select.right1");
    var right2 = chooser.find("select.right2");
    var left1Arrow = chooser.find(".left1-arrow");
    var left2Arrow = chooser.find(".left2-arrow");
    var right1Arrow = chooser.find(".right1-arrow");
    var right2Arrow = chooser.find(".right2-arrow");
    
    var canMoveTo = (left.val() || []).length > 0;
    var canMoveFrom = (right1.val() || right2.val()|| []).length > 0;
    
    left1Arrow.toggleClass("muted", !canMoveFrom);
    right1Arrow.toggleClass("muted", !canMoveTo);
    left2Arrow.toggleClass("muted", !canMoveFrom);
    right2Arrow.toggleClass("muted", !canMoveTo);
  }
  
  function move(chooser, source, dest) {
    chooser = $(chooser);
    var selected = chooser.find(source).children("option:selected");
    var dest = chooser.find(dest);
    dest.children("option:selected").each(function(i, e) {e.selected = false;});
    dest.append(selected);
    updateChooser(chooser);
    chooser.trigger("change");
  }
  
  $(document).on("change", ".chooser select", function() {
    updateChooser($(this).parents(".chooser"));
  });
  
  $(document).on("click", ".chooser .right1-arrow", function() {
    move($(this).parents(".chooser"), ".left", ".right1");
  });
  
  $(document).on("click", ".chooser .right2-arrow", function() {
    move($(this).parents(".chooser"), ".left", ".right2");
  });
  
  $(document).on("click", ".chooser .left1-arrow", function() {
    move($(this).parents(".chooser"), ".right1", ".left");
  });
  
  $(document).on("click", ".chooser .left2-arrow", function() {
    move($(this).parents(".chooser"), ".right2", ".left");
  });
  
  $(document).on("dblclick", ".chooser select.right1", function() {
    move($(this).parents(".chooser"), ".right1", ".left");
  });
  
  $(document).on("dblclick", ".chooser select.right2", function() {
    move($(this).parents(".chooser"), ".right2", ".left");
  });
  
  var binding = new Shiny.InputBinding();
  
  binding.find = function(scope) {
    return $(scope).find(".chooser");
  };
  
  binding.initialize = function(el) {
    updateChooser(el);
  };
  
  binding.getValue = function(el) {
    return {
      left: $.makeArray($(el).find("select.left option").map(function(i, e) { return e.value; })),
      right1: $.makeArray($(el).find("select.right1 option").map(function(i, e) { return e.value; })),
      right2: $.makeArray($(el).find("select.right2 option").map(function(i, e) { return e.value; }))
    }
  };
  
  binding.setValue = function(el, value) {
    // TODO: implement
  };
  
  binding.subscribe = function(el, callback) {
    $(el).on("change.chooserBinding", function(e) {
      callback();
    });
  };
  
  binding.unsubscribe = function(el) {
    $(el).off(".chooserBinding");
  };
  
  binding.getType = function() {
    return "shinyjsexamples.chooser";
  };
  
  Shiny.inputBindings.register(binding, "shinyjsexamples.chooser");
  
})();