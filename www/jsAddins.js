// "Enter"", "ctrl+z" and "ctrl+R"" key listener.
$(document).on("keyup", function(e){
  if(e.keyCode == 13){
    Shiny.onInputChange("enterPressed", Math.random());
  }
  if(e.key == "z" && e.ctrlKey){
    Shiny.onInputChange("ctrlzPressed", Math.random());
  }
  if(e.key =="r" && e.ctrlKey){
    Shiny.onInputChange("ctrlrPressed", Math.random());
  }
});


// Return the X & Y coordinates where mouse is clicked and the size of the chessboard.
// The coordinates are referenced taking the top left corner of the board as the origin point.
$(document).on("click", function(e){
  var offset = $(board).offset();
  var posX = e.clientX - offset.left + document.documentElement.scrollLeft - document.documentElement.offsetLeft;
  var posY = e.clientY - offset.top + document.documentElement.scrollTop - document.documentElement.offsetTop;
  var height = $(board).height();
  Shiny.onInputChange("mouseClicked", [posX, posY, height]);
});

// Double-click listener.
$(document).on("dblclick", function(e){
  Shiny.onInputChange("dblClick", Math.random())
});
