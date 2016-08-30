// to install: npm install mathjax-node zerorpc svg2png

try {
    var zerorpc = require("zerorpc");

    var mathjax = require("mathjax-node/lib/mj-single.js");
    mathjax.config({
        MathJax: {
            // traditional MathJax configuration
        }
    });
    mathjax.start();

    var svg2png = require("svg2png");

    var server = new zerorpc.Server({
        mathml_to_svg: function(mathml, reply) {
            mathjax.typeset({
                math: mathml,
                format: "MathML",
                svg: true,
            }, function (data) {
                if (!data.errors) {
                    reply(null, data.svg);
                }
            });
        },
        rasterize: function(svg, reply) {
            svg2png(Buffer.from(svg, 'utf8'), {
                width: 300,
                height: 400
            })
            .then(buffer => reply(null, buffer))
            .catch(e => console.error(e));
        }
    });

    console.log('OK')

    server.bind("tcp://0.0.0.0:4241");
} catch (ex) {
    console.log('FAIL')
    console.log(ex)
}
