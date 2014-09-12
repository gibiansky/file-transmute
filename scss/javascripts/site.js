// Whether the Try-It button has been pressed.
var conversionShown = false;

$(document).ready(function() {
    $("#try-it").on('click', function(event) {
        event.preventDefault();
        if(!conversionShown) {
            $("#upload").attr('class', 'upload narrow');
            $(".upload-separator").attr('class', 'upload-separator no-margin');
        } else {
            $("#upload-reminder").attr('class', 'border');
        }
    });
    $("#upload").on('transitionend webkitTransitionEnd', function() {
        $("#upload").css('overflow', 'visible');
        conversionShown = true;
    });
    $("#upload-reminder").on('transitionend webkitTransitionEnd', function() {
        $("#upload-reminder").attr('class', 'no-border');
    });
});
