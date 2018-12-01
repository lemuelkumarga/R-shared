$(document).ready(function(){

  /* ===================================
    PPT HIGHCHARTS
    THIS IS USED TO FIX HIGHCHARTS
    TOOLTIP ERROR WHEN AUTO ZOOMING IS 
    ENABLED
    https://stackoverflow.com/questions/20339047/reveal-js-with-highcharts
  ===================================== */
  if ((typeof Highcharts) != "undefined") {
    (function (H) {
        H.wrap(H.Pointer.prototype, 'normalize', function (proceed, e) {
            var e = proceed.call(this,e);
            var zoom = $('body').css('zoom');
            var positionX = e.pageX - e.chartX;
            var positionY = e.pageY - e.chartY;
            e.chartX = Math.round((e.pageX - positionX*zoom)/zoom);
            e.chartY = Math.round((e.pageY - positionY*zoom)/zoom);
          return e;
        });
    }(Highcharts));
  }
});