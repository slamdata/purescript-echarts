var data  = [                 {name: "Beijing", value: 12},
                 {name: "Tianjin", value: 12},
                 {name: "Shanghai", value: 12},
                 {name: "Chongqing", value: 12},
                 {name: "Hebei", value: 12},
                 {name: "Henan", value: 12},
                 {name: "Yunnan", value: 12},
                 {name: "Liaoning", value: 12},
                 {name: "Heilongjiang", value: 12},
                 {name: "Hunan", value: 12},
                 {name: "Anhui", value: 12},
                 {name: "Shandong", value: 12},
                 {name: "Xinjiang", value: 12},
                 {name: "Jiangsu Province", value: 12},
                 {name: "Zhejiang", value: 12},
                 {name: "Jiangxi", value: 12},
                 {name: "Hubei", value: 12},
                 {name: "Guangxi", value: 12},
                 {name: "Gansu", value: 12},
                 {name: "Shanxi", value: 12},
                 {name: "Inner", value: 12},
                 {name: "Shaanxi", value: 12},
                 {name: "Jilin", value: 12},
                 {name: "Fujian", value: 12},
                 {name: "Guizhou", value: 12},
                 {name: "Guangdong", value: 12},
                 {name: "Qinghai", value: 12},
                 {name: "Tibet", value: 12},
                 {name: "Sichuan", value: 12},
                 {name: "Ningxia", value: 12},
                 {name: "Hainan", value: 12},
                 {name: "Taiwan", value: 12},
                 {name: "Hong Kong", value: 12},
                 {name: "Macau", value: 12}
         ];
var strings = ""
for (var i = 0 ; i < data.length; i++) {
    var datum = data[i];
    var randomX = Math.random() * 3;
    var randomY = Math.random() * 5;
    var x = 100 + randomX;
    var y = 34 + randomY;
    strings += "Tuple \"" + datum.name + "\" (Tuple " + x.toFixed(4) + " " + y.toFixed(4) + "),\n";
}

console.log(strings);
