
const funcs = [];

function determContext(before) {
    if (/\#/.test(before))
        return 'comment';
    if (/([A-Za-z][A-Za-z0-9]*)(\:\:\:?)$/.test(before))
        return 'ns';
    if (/,[\s]*$/.test(before))
        return '';
    if (/['"].*$/.test(before))
        return 'string';
    return '';
}

module.exports = function(columnSource) {
    this._columnSource = columnSource;
    this.identifierRegexps = [/[A-Za-z0-9_\.\$]/];
    this.getCompletions = (editor, session, pos, prefix, callback) => {

        let row = pos.row;
        let col = pos.column;
        let line = session.getDocument().getLine(row);
        let before = line.substring(0, col - prefix.length);
        let context = determContext(before);

        if (context === 'comment' || context === 'string') {
            callback(context);
            return;
        }

        let entries = funcs;

        entries = entries.filter((entry) => {
            return entry.value.toLowerCase().startsWith(prefix.toLowerCase());
        });

        Promise.resolve().then(() => {

            if (context == 'ns') {
                let match = before.match(/([A-Za-z][A-Za-z0-9]*)(\:\:\:?)$/);
                let ns = match[1];
                let dots = match[2];
                entries = entries.filter(entry => entry.ns === ns);
            }
            else {

                return this._columnSource().then((vars) => {

                    let vars1 = vars.map(v => {
                        return {
                            name: "'" + v + "'",
                            value: "`" + v + "`",
                            meta: 'variable',
                            type: 'var' }});

                    let vars2 = vars.map(v => {

                        if ( ! /^[A-Za-z][A-Za-z0-9\._]*$/.test(v))
                            v = '`' + v + '`';

                        return {
                            name:   v,
                            value:  v,
                            meta: 'variable',
                            type: 'var' }});

                    entries = entries.concat(vars1).concat(vars2);
                });
            }
        }).then(() => {

            let index = 0;

            entries = entries.map((entry) => {
                // custom completer
                entry.completer = this;
                // provide scores so it retains alphabetic sort
                let score = entries.length - index++;
                if (entry.value.startsWith(prefix))
                    score += 1000;
                entry.score = score;
                return entry;
            });

            callback(null, entries);
        }, () => {});
    };
    this.insertMatch = (editor, entry) => {
        if (entry.type === 'ns') {
            editor.completer.insertMatch({value: entry.value});
            setTimeout(() => editor.execCommand("startAutocomplete"), 0);
        }
        else {
            editor.completer.insertMatch({value: entry.value});
        }
    };
};
