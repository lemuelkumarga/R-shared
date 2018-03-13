$(document).ready(function(){

    

    $('[data-toggle="popover"]').popover({ trigger : "hover focus", 
    									   placement : function (context, source) {
	    									   				var win_y = $(document).scrollTop() + $(window).height();
	    									   				var win_x = $(window).width();
	    									   				var position = $(source).position();
	    									   				if (win_x - position.left > 500) {
	    									   					return "right"
	    									   				} else if (position.left > 500) {
	    									   				 	return "left"
	    									   				} else if (win_y - position.top < 500) {
	    									   				 	return "top"
	    									   				} else {
	    									   				 	return "bottom"
	    									   				}
													    } 
										}); 

});