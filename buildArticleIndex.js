const fs = require("fs");
const path = require("path");
const { Elm } = require("./articleIndexBuilder.js");

const app = Elm.ArticleIndex.Builder.init();

app.ports.sendError.subscribe((error) => {
    console.error(error);
    process.exit(1);
});

app.ports.sendResult.subscribe((result) => {
    fs.writeFile(
        "public/articleIndex.json",
        result.replace(/\\\\/g, "/"),
        (err) => {
            if (err) {
                console.error("Error writing file:", err);
            } else {
                console.log("File is written successfully!");
            }
        }
    );
});

function readFiles(dirPath) {
    fs.readdir(dirPath, (err, files) => {
        if (err) {
            console.error("Could not list the directory.", err);
            process.exit(1);
        }

        files.forEach((file, index) => {
            const filePath = path.join(dirPath, file);

            fs.stat(filePath, (err, stat) => {
                if (err) {
                    console.error("Error stating file.", err);
                    process.exit(1);
                }

                if (stat.isDirectory()) {
                    readFiles(filePath);
                } else {
                    fs.readFile(filePath, "utf8", (err, content) => {
                        if (err) {
                            console.error("Error reading file:", err);
                            process.exit(1);
                        }

                        app.ports.receiveInput.send({
                            relativePath: filePath.replace(/public[\\\/]/, ""),
                            content,
                        });

                        if (index === files.length - 1) {
                            app.ports.receiveEndOfInput.send(null);
                        }
                    });
                }
            });
        });
    });
}

readFiles("public/articles");
