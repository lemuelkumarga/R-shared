$(document).ready(function(){

  $('[data-toggle="popover"]').popover({ trigger : "hover focus", 
                                         placement : function (context, source) {
                                                          var win_y = $(document).scrollTop() + $(window).height();
                                                          var win_x = $(window).width();
                                                          var position = $(source).position();
                                                          if (win_x - position.left > 700) {
                                                              return "right"
                                                          } else if (position.left > 700) {
                                                              return "left"
                                                          } else if (win_y - position.top < 700) {
                                                              return "top"
                                                          } else {
                                                              return "bottom"
                                                          }
                                                      } 
                                      }); 

  /* Allow for popovers to be dismissed in mobile 
  Courtesy of gregblass.
  https://github.com/twbs/bootstrap/issues/16028 */
  function closePopovers() {
    $('.popover').popover('hide');
    $(".main-container").unbind("click", closePopovers); 
  };

  // Bootstrap popovers don't close on outside click
  // https://github.com/twbs/bootstrap/issues/16028
  // https://bugs.webkit.org/show_bug.cgi?id=151933
  $('[data-toggle="popover"]').on('shown.bs.popover', function () {
    setTimeout(function() {
      $(".main-container").bind("click", closePopovers); 
    }, 0);
  });

  // Bootstrap popovers require two clicks after hide
  // https://github.com/twbs/bootstrap/issues/16732
  $('body').on('hidden.bs.popover', function (e) {
    $(e.target).data("bs.popover").inState.click = false;
  });

});