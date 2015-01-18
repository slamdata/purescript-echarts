var gulp = require("gulp"),
    fs = require("fs");


module.exports = function(taskName, module, entry, distDir) {
    if (taskName === undefined) {
        taskName = "make-script";
    }
    if (distDir === undefined) {
        distDir = "dist";
    }
    if (module === undefined) {
        module = "Main";
    }
    if (entry === undefined) {
        entry = "main.js";
    }
    gulp.task(taskName, function() {
        var run = function() {
            var path = distDir + "/" + entry;
            var content = "require('" + module + "').main()";
            fs.writeFileSync(path, content);
        };
        fs.stat(distDir, function(err, stats) {
            if (err) {
                fs.mkdir(distDir, function(err) {
                    if (err) throw err;
                    run();
                });
            }
            run();
        });

    });
};
