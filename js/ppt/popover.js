$(document).ready(function(){

  /* ===================================
  	PPT POPOVERS
  ===================================== */
  $('[data-toggle="popover"]').popover({ trigger : "hover focus" ,
                                          container: 'body',
                                          placement : function (context, source) {
                                                          var slide = $('slide.current')
                                                          var s_x = slide.offset().left
                                                          var s_y = slide.offset().top
                                                          var s_w = slide.width()
                                                          var s_h = slide.height()
                                                          var l_x = $(source).offset().left;
                                                          var l_y = $(source).offset().top;

                                                          if ((l_y - s_y) / s_h < 0.5) {
                                                            return "bottom"
                                                          } else {
                                                              return "top"
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

});