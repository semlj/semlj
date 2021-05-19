
let added = false;

function tokenize(row) {
    var data = this.data[row];
    var tokens = [];
    if (!data)
        return tokens;
    if (typeof data == "string")
        data = {value: data};
    if (!data.caption)
        data.caption = data.value || data.name;

    tokens.push({ value: ' ', type: 'icon_' + (data.type ? data.type : 'none') });

    var last = -1;
    var flag, c;
    for (var i = 0; i < data.caption.length; i++) {
        c = data.caption[i];
        flag = data.matchMask & (1 << i) ? 1 : 0;
        if (last !== flag) {
            tokens.push({type: data.className || "" + ( flag ? "completion-highlight" : ""), value: c});
            last = flag;
        } else {
            tokens[tokens.length - 1].value += c;
        }
    }

    if (data.meta) {
        var maxW = this.renderer.$size.scrollerWidth / this.renderer.layerConfig.characterWidth;
        var metaData = data.meta;
        if (metaData.length + data.caption.length > maxW - 2) {
            metaData = metaData.substr(0, maxW - data.caption.length - 3) + "\u2026";
        }
        tokens.push({type: "rightAlignedText", value: metaData});
    }
    return tokens;
}

module.exports = {
    add: function(editor) {
        if (added === true)
            return;

        if (editor.completer) {
            let popup = editor.completer.getPopup();
            let renderer = popup.renderer;
            let tokeniz = tokenize.bind(popup);
            popup.session.bgTokenizer.$tokenizeRow = tokeniz;

            let updateCharacterSize = renderer.updateCharacterSize;
            updateCharacterSize = updateCharacterSize.bind(renderer);
            renderer.updateCharacterSize = function() {
                updateCharacterSize();
                this.lineHeight = 20;
            }.bind(renderer);

            added = true;
        }
    }
};

