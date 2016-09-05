// to install: npm install mathjax-node svg2png

try {
    function server(methods) {
        net = require('net');

        var uint32 = {
            parse: function(buffer) {
                return (buffer[0] << 24) |
                    (buffer[1] << 16) |
                    (buffer[2] << 8) |
                    (buffer[3] << 0);
            },
            make: function(x) {
                var buffer = new Buffer(4);
                buffer[0] = x >> 24;
                buffer[1] = x >> 16;
                buffer[2] = x >> 8;
                buffer[3] = x >> 0;
                return buffer;
            }
        };

        var server = net.createServer(function (socket) {
            function write(data) {
                var json = JSON.stringify(data);
                var size = json.length;
                socket.write(Buffer.concat([uint32.make(size), new Buffer(json)]));
            }

            var state = {
                buffer: new Buffer(0)
            };

            function rpc(size) {
                var json = JSON.parse(state.buffer.slice(4, size + 4));
                state.buffer = state.buffer.slice(size + 4)
                var method = methods[json.call];
                if (method) {
                    try {
                        method.apply(null, json.args.concat([write]));
                    } catch(e) {
                        write({error: e.toString() + '; ' + e.stack});
                    }
                }
            }

            socket.on('close', function() {
                // means our Python client has lost us. quit.
                process.exit();
            });

            socket.on('data', function(data) {
                state.buffer = Buffer.concat(
                    [state.buffer, data]);

                if (state.buffer.length >= 4) {
                    var buffer = state.buffer;
                    var size = uint32.parse(buffer);
                    if (buffer.length >= size + 4) {
                        rpc(size);
                    }
                }
            });
        });

        server.on('listening', function() {
            var port = server.address().port;
            process.stdout.write('HELLO:' + port.toString() + '\n');
        });

        server.listen(0);  // pick a free port
    }

    var mathjax = require("mathjax-node/lib/mj-single.js");
    mathjax.config({
        MathJax: {
            // traditional MathJax configuration
        }
    });
    mathjax.start();

    server({
        mathml_to_svg: function(mathml, reply) {
            mathjax.typeset({
                math: mathml,
                format: "MathML",
                svg: true
            }, function (data) {
                if (!data.errors) {
                    reply({data: data.svg});
                } else {
                    reply({error: data.errors});
                }
            });
        },
        rasterize: function(svg, size, reply) {
            var svg2png = require("svg2png");

            svg2png(Buffer.from(svg, 'utf8'), {
                width: size[0],
                height: size[1]
            })
            .then(function(buffer) {
                reply({data: buffer});
            })
            .catch(function(e) {
                reply({error: e.toString()});
            });
        }
    });
} catch (ex) {
    process.stdout.write('FAIL.' + '\n' + ex.toString() + '\n');
}
