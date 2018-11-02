$(document).ready(function(){


  /* ===================================
  	AUTO ZOOM BASED ON WIDTH
  ===================================== */
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
  }

  // Resize PPT based on window change
  $(window).on('resize orientationchange', function() {
  	ppt_resize()
  })

  ppt_resize()


  /* ===================================
  	PPT POPOVERS
  ===================================== */
  $('[data-toggle="popover"]').popover({ trigger : "hover focus" ,
                                          placement : function (context, source) {
                                                          var win_y = $(document).scrollTop() + $(window).height();
                                                          var win_x = $(window).width();
                                                          var position = $(source).position();
                                                          if (win_y - position.top >= $(window).height() / 2) {
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