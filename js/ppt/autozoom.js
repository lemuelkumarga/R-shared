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
  	ppt_resize();
  })


  ppt_resize();

});