var gulp = require('gulp')
, purescript = require('gulp-purescript')
, browserify = require("gulp-browserify")
, concat = require('gulp-concat')
, run = require('gulp-run')
, sequence = require('run-sequence')
, jsValidate = require('gulp-jsvalidate')
, rename = require('gulp-rename')
; 

require("./gulp/serve.js")();
require("./gulp/runner.js")("runner", "Main");

var sources = [
    'src/**/*.purs',
    'bower_components/purescript-*/src/**/*.purs'
];

var foreigns = [
    'src/**/*.js',
    'bower_components/purescript-*/src/**/*.js'
];

var exampleSources = [
    'example/**/*.purs',
    'example/bower_components/purescript-*/src/**/*.purs'
];

var exampleForeigns = [
    'example/**/*.js',
    'example/bower_components/purescript-*/src/**.js'
];


gulp.task('docs', function() {
    var modules = [
        "AddData",
        "Axis",
        "Chart",
        "Color",
        "Common",
        "Connect",
        "Coords",
        "DataRange",
        "DataZoom",
        "Effects",
        "Events",
        "Formatter",
        "Grid",
        "Image",
        "Item.Data",
        "Item.Value",
        "Legend",
        "Loading",
        "Mark.Data",
        "Mark.Effect",
        "Mark.Line",
        "Mark.Point",
        "Options",
        "RoamController",
        "Series",
        "Series.EventRiver",
        "Series.Force",
        "Series.Gauge",
        "Style.Area",
        "Style.Checkpoint",
        "Style.Chord",
        "Style.Item",
        "Style.Line",
        "Style.Link",
        "Style.Node",
        "Style.Text",
        "Symbol",
        "Timeline",
        "Title",
        "Toolbox",
        "Tooltip",
        "Utils"
    ];
    var docgen = {};
    for (var i = 0; i < modules.length; i++) {
        docgen["ECharts." + modules[i]] = "docs/Echarts/" + modules[i] + ".md";
    }
    return purescript.pscDocs({
        src: sources,
        docgen: docgen
    });
});

gulp.task("lib", function() {
    return purescript.psc({
        src: sources,
        ffi: foreigns
    });
});

gulp.task("make", ["runner"], function() {
    return purescript.psc({
        src: sources.concat(exampleSources),
        ffi: foreigns.concat(exampleForeigns),
        output: "output"
    });
});

gulp.task("bundle", ["make"], function() {
    return purescript.pscBundle({
        src: "output/**/*.js",
        main: "Main",
        output: "dist/main.js"
    });
});


gulp.task("browserify", ["bundle"], function() {
    gulp.src("dist/main.js")
        .pipe(browserify({}))
        .pipe(rename("dist/build.js"))
        .pipe(gulp.dest("."));
});

gulp.task("concat", ["browserify"], function() {
    gulp.src(["bower_components/echarts/build/source/echarts-all.js",
              "dist/build.js"])
        .pipe(concat("build.js"))
        .pipe(gulp.dest("public"));
});
            

gulp.task('psci', function() {
    return purescript.psci({
        src: sources,
        ffi: foreigns
    }).pipe(gulp.dest("."));
});



gulp.task("default", function() {
    sequence("concat", "docs", "serve", function() {
        console.log("**************************************************");
        console.log(" To see examples navigate to http://localhost:5050");
        console.log("**************************************************");
    });
});
