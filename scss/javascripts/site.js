// Whether the Try-It button has been pressed.
var conversionShown = false;

$(document).ready(function() {
    /*
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
    */

    $("#upload-reminder").attr('class', 'no-border');
    $("#upload").css('overflow', 'visible');
    $("#upload").attr('class', 'upload narrow');
    $(".upload-separator").attr('class', 'upload-separator no-margin');

    $("#feedback-submit").click(function(e) {
        var message = $("#feedback-textarea").val();
        var email = $("#feedback-email").val();
        var params = {"message" : message};
        if(email != "") {
            params["email"] = email;
        }
        $.post("/feedback", params);
    });

    $("#go-convert").click(function(e) {
        var from = "pdf";
        var to = "hello";
        var params = {"from": from, "to": to};
        $.ajax({
            url: "/convert",
            dataType: 'json',
            type: "POST",
            contentType: "application/json; charset=utf8",
            data: params,
            success: function(resp) {
                console.log(resp);
            },
        });
    });
});
