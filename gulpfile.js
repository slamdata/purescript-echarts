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

var paths = {
    src: 'src/**/*.purs',
    bowerSrc: [
      'bower_components/purescript-*/src/**/*.purs',
      'bower_components/purescript-*/src/**/*.purs.hs'
    ],
    dest: '',
    docs: {
        'all': {
            dest: 'MODULES.md',
            src: [
              'src/**/*.purs'
            ]
        }
    },
    example: ['example/**/*.purs',
              "example/bower_components/purescript-*/src/**/*.purs"],
    test: 'test/**/*.purs'
};

var options = {
    test: {
        main: 'Test.Main',
        output: 'output/test.js'
    }
};

function handleError(source) {
    source.on("error", function(e) {
        console.error("\u0007");
        console.error(e.message);
        source.end();
    });
}


function docs (target) {
    return function() {
        var docgen = purescript.pscDocs();
        handleError(docgen);

        return gulp.src(paths.docs[target].src)
            .pipe(docgen)
            .pipe(gulp.dest(paths.docs[target].dest));
    };
}
gulp.task('docs', docs('all'));

gulp.task("prod", function() {
    var psc = purescript.psc({
        output: "build.js",
        modules: ["Main"],
        main: "Main"
    });
    handleError(psc);
    return gulp.src(
        [paths.src].concat(paths.example).concat(paths.bowerSrc)
    ).pipe(psc).pipe(gulp.dest("public"));
});

gulp.task("make-dev", function() {
    var psc = purescript.pscMake({
        output: "dist/node_modules"
    });
    handleError(psc);
    return gulp.src(
        [paths.src].concat(paths.example).concat(paths.bowerSrc)
    ).pipe(psc);
});

gulp.task("bundle-dev", function() {
    gulp.src("dist/main.js")
        .pipe(browserify({}))
        .pipe(rename("example.js"))
        .pipe(gulp.dest("public"));
});

gulp.task('compile-dev', function(cb) {
    sequence('runner', 'make-dev', 'bundle-dev', cb);
});

gulp.task("watch-dev", function() {
    gulp.watch([paths.src, paths.example], ['compile-dev', 'docs']);
});



gulp.task("dev", ['compile-dev', 'docs', 'serve', 'watch-dev']);

gulp.task('psci', function() {
    gulp.src([paths.src].concat(paths.example).concat(paths.bowerSrc))
        .pipe(purescript.dotPsci({}));
});

gulp.task("default", function() {
    sequence("prod", "docs", "serve");
});
