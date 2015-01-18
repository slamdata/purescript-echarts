var express = require("express"),
    gulp = require("gulp"),
    http = require("http"),
    ecstatic = require("ecstatic");

module.exports = function(taskName, serverPort, subdirectory) {
    if (taskName === undefined) {
        taskName = "serve";
    }
    if (serverPort === undefined) {
        serverPort = 5050;
    }
    if (subdirectory === undefined) {
        subdirectory = "public";
    }
    var app = express();
    console.log(__dirname + '/../../bower_components');
    app.use(express.static(__dirname + '/../' + subdirectory));
    app.use("/lib", express.static(__dirname + '/../bower_components'));

    gulp.task(taskName, function() {
        app.listen(serverPort);
    });
};
