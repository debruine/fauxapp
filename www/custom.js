$(document).on("shiny:connected", function() {
  // send window width to shiny
  shiny_size = function() {
    Shiny.setInputValue("window_width", window.innerWidth);
    Shiny.setInputValue("window_height", window.innerHeight);
  }

  window.onresize = shiny_size;
  shiny_size(); // trigger once at start

  // collapse box by ID
  closeBox = function(boxid) {
    var box = $('#' + boxid).closest('.box');
    if (!box.hasClass('collapsed-box')) {
      box.find('[data-widget=collapse]').click();
    }
  };

  // uncollapse box by ID
  openBox = function(boxid) {
    var box = $('#' + boxid).closest('.box');
    if (box.hasClass('collapsed-box')) {
      box.find('[data-widget=collapse]').click();
    }
  };

  // toggle box on click
  $('.box').on('click', '.box-header h3', function() {
    $(this).closest('.box')
           .find('[data-widget=collapse]')
           .click();
  });

  // update input when curent_factors button clicked
  $("#current_factors").on('click', 'button', function() {
    var val = $(this).text();

    Shiny.setInputValue("edit_factor", val, {priority: "event"});
  });

  $("#current_random_factors").on('click', 'button', function() {
    var val = $(this).text();

    Shiny.setInputValue("edit_random_factor", val, {priority: "event"});
  });

  $("#current_fixed_factors").on('click', 'button', function() {
    var val = $(this).text();

    Shiny.setInputValue("edit_fixed_factor", val, {priority: "event"});
  });

});
