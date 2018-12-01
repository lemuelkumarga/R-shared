$(document).ready(function(){


  /* ===================================
    STORE HEIGHT OF EACH
    HIGHCHART FOR ZOOMING LATER ON
  ===================================== */
  $('.highchart').each(function() {
    $(this).attr("origin_height", $(this).height())
  })

  /* ===================================
  	AUTO ZOOM BASED ON WIDTH

    (Currently Disabled Due to 
    Interactive Chart Issues)
    
  function ppt_resize() {

  	var width = $('slides > slide').css('width').replace('px','')
  	var height = $('slides > slide').css('height').replace('px','')
  	var asp_ratio = height / width

  	var wwidth = $(window).innerWidth()
  	var wheight = $(window).innerHeight()
  	var wasp_ratio = wheight / wwidth

  	// Fix the Width
  	if (asp_ratio < wasp_ratio) {
  		wheight = wwidth * asp_ratio

  	} else {
  		wwidth = wheight * 1. / asp_ratio
  	}

  	$('body').css('zoom', 0.95 * wwidth / width)

    // To fix highcharts
    //$(".highchart").css('zoom', 1. / (0.95 * wwidth / width))
    //$('.highchart').each(function() {
    //  $(this).css('height', $(this).attr('origin_height') / $(this).css('zoom'))
    //})
  }

  // Resize PPT based on window change
  $(window).on('resize orientationchange', function() {
  	ppt_resize()
  })


  ppt_resize()
  ===================================== */

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