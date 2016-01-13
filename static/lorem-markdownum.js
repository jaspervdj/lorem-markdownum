$(document).ready(function() {
    function isShowAdvanced() {
        return $('#show-advanced').is(':checked');
    }

    function isPreviewHtml() {
        return $('#preview-html').is(':checked');
    }

    function pokePreviewHtml() {
        if (isPreviewHtml()) {
            $('#markdown-html pre.markdown').hide();
            $('#markdown-html div.html').show();
        } else {
            $('#markdown-html div.html').hide();
            $('#markdown-html pre.markdown').show();
        }
    }

    $('#show-advanced').on('change', function() {
        if (isShowAdvanced()) {
            $('#advanced').slideDown();
        } else {
            $('#advanced').slideUp();
        }
    });

    $('#preview-html').on('change', function() {
        pokePreviewHtml();
    });

    $('#form-generate').submit(function() {
        $('#loading').show();
        $('#markdown-html').hide();

        var inputIds = [
                '#no-headers',
                '#no-code',
                '#no-quotes',
                '#no-lists',
                '#no-inline-markup',
                '#reference-links',
                '#underline-headers',
                '#underscore-em',
                '#underscore-strong',
                '#num-blocks'];

        // Form processing
        var query = {};
        for (var i = 0; i < inputIds.length; i++) {
            var $input = $(inputIds[i]);
            var name   = $input.attr('name');
            if ($input.attr('type') === 'checkbox') {
                if ($input.is(':checked')) {
                    query[name] = 'on';
                }
            } else if ($input.attr('type') === 'text') {
                if ($input.val().length > 0) {
                    query[name] = $input.val();
                }
            }
        }

        $.get('markdown-html.html', query, function(data) {
            $('#loading').hide();
            $('#markdown-html').html(data);
            pokePreviewHtml();
            $('#markdown-html').slideDown();
        });

        return false;
    });
});
