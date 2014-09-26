$(document).ready(function() {
    $("#feedback-submit").click(function(e) {
        var message = $("#feedback-textarea").val();
        var email = $("#feedback-email").val();
        var params = {"message" : message};
        if(email != "") {
            params["email"] = email;
        }
        $.post("/feedback", params);
    });

    var files = undefined;
    $("#file-upload").change(function(e) {
        files = e.target.files;
        $("#filename").text(escape(files[0].name));
        $("#browse").text("Change file");
    });

    $("#go-convert").click(function(e) {
        var from = "pdf";
        var to = "jpg";

        var params = new FormData();
        params.append("from", from);
        params.append("to", to);
        params.append("file", files[0]);

        // Switch to loading gif.
        $("#download").html('<img style="padding-right: 1em;" src="/static/imgs/loading.gif"/><b>Loading...</b>');

        $.ajax({
            url: "/convert",
            type: "POST",
            data: params,
            contentType: false,
            processData: false,
            success: function(resp) {
                var status = resp.status;
                if(status == "success") {
                    var url = resp.url;
                    $("#download").html('<a href="' + url + '"><b>Download file</b></a>');
                } else {
                    $("#download").html('<b>Conversion failed.</b>');
                }
            },
        });
    });

    $(".dropdown-menu li a").click(function() {
        $(this).parents(".btn-group").find('.filetype').text($(this).text());
        $(this).parents(".btn-group").find('.btn').val($(this).data('value'));
    });
});
