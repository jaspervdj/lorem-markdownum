"use strict";

document.addEventListener("DOMContentLoaded", function() {
    function pokePreviewHtml() {
        const markdown = document.querySelector("#markdown-html pre.markdown");
        const html = document.querySelector("#markdown-html div.html");
        const preview = document.getElementById("preview-html").checked;
        markdown.style.display = preview ? "none" : "block";
        html.style.display = preview ? "block" : "none";
    }

    const advancedToggle = document.getElementById("show-advanced");
    function pokeAdvanced() {
        const advanced = document.getElementById("advanced");
        advanced.style.display = advancedToggle.checked ? "flex" : "none";
    };
    advancedToggle.addEventListener("change", pokeAdvanced);
    pokeAdvanced();

    document.getElementById("preview-html").addEventListener("change", function() {
        pokePreviewHtml();
    });

    document.getElementById("form-generate").addEventListener("submit", function(event) {
        event.preventDefault();

        const dst = document.getElementById("markdown-html");
        const loading = document.getElementById("loading");
        dst.style.display = "none";
        loading.style.display = "block";

        const inputIds = [
            "no-headers",
            "no-code",
            "no-quotes",
            "no-lists",
            "no-inline-markup",
            "reference-links",
            "no-external-links",
            "underline-headers",
            "underscore-em",
            "underscore-strong",
            "num-blocks",
            "no-wrapping",
            "fenced-code-blocks"
        ];

        // Form processing
        const query = {};
        for (const inputId of inputIds) {
            const input = document.getElementById(inputId);
            if (input.type === "checkbox") {
                if (input.checked) {
                    query[input.name] = "on";
                }
            } else if (input.type === "text") {
                if (input.value.length > 0) {
                    query[name] = input.value;
                }
            }
        }

        const url = new URL("markdown-html.html", window.location.href);
        url.search = new URLSearchParams(query).toString();
        const request = new XMLHttpRequest();
        request.addEventListener("load", function() {
            dst.innerHTML = request.responseText;
            pokePreviewHtml();
            dst.style.display = "block";
            loading.style.display = "none";
        });
        request.open("GET", url);
        request.send();
    });

    document.getElementById("copy").addEventListener("click", function(event) {
        event.preventDefault();
        const markdown = document.querySelector("#markdown-html pre.markdown")
        navigator.clipboard.writeText(markdown.innerText);
    });
});
