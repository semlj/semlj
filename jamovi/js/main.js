
const ace = require('brace');

require('brace/mode/r');
require('brace/ext/language_tools');

require('./css');
const Suggest = require('./suggest');
const SuggestIcons = require('./suggesticons');



const events = {
    	editor_creating(ui) {

	    this.editSessions = { };

	  //  let $contents = ui.view.$el;
   	  let $contents=ui.syntax.$el;
   	  console.log("creating editor");
   	  console.log($contents);
	    $contents.css('display', 'flex');
	    $contents.css('flex-direction', 'column');

		$contents.append(`
		    <div id="editor-box">
		        <div id="toolbar">
		            <div id="config" title="Configuration"></div>
		            <div id="run" title="Run"></div>
		        </div>
		        <div id="editor"></div>
                <div id="info">Ctrl + Shift + Enter to run</div>
            </div>`);

        let $config = $contents.find('#config');
        $config.append(`
            <div id="menu">
                <label id="r-label">R Version</label>
                <select id="r-version">
                    <option value="bundled">jamovi R</option>
                    <option value="external">System R</option>
                </select>
                <label id="output-label">Output</label>
                <select id="output">
                    <option value="noEcho">Show output only</option>
                    <option value="echo">Show code and output</option>
                </select>
                <div id="figure-size">
                    <div id="figure-size-title">Figure size (px)</div>
                    <label>Width</label>
                    <input id="figure-width" placeholder="Default">
                    <label>Height</label>
                    <input id="figure-height" placeholder="Default">
                </div>
            </div>`);

		this.$editor = $contents.find('#editor');
		this.$run = $contents.find('#run');
		this.$menu = $contents.find('#menu');

		this.$run.on('click', () => this.run(ui));

		this.$output = $config.find('#output');
		this.$output.on('change', (event) => {
		    ui.output.setValue(this.$output.val());
		});

		this.$r = $config.find('#r-version');
		this.$r.on('change', (event) => {
		    ui.R.setValue(this.$r.val());
		});

		this.$figWidth = $config.find('#figure-width');
		this.$figHeight = $config.find('#figure-height');

		this.$menu.find('input').on('keyup', (event) => {
		    if (event.keyCode == 13)
		        this.run(ui);
		});

		$config.on('click', (event) => {
		    if (event.target === $config[0])
		        this.toggleMenu(ui);
		});

		this.$editor.on('click', () => {
            this.hideMenu(ui);
		});

		if (navigator.platform === 'MacIntel') {
		    let $info = $contents.find('#info');
		    $info.text('\u2318 + Shift + Enter to run');
		}
		


},

     loaded(ui) {
        console.log("loaded code");

        this.editor = ace.edit('editor');
        this.editor.$blockScrolling = Infinity; // disable a warning
        this.editor.setShowPrintMargin(false);
        this.editor.setHighlightActiveLine(false);
        this.editor.focus();

        this.editor.setOptions({
            enableBasicAutocompletion: true,
        });

        this.editor.commands.on('afterExec', (event) => {
            let editor = event.editor;
            if (event.command.name == 'insertstring') {
                let position = editor.getCursorPosition();
                let line = editor.getSession().getDocument().getLine(position.row);
                let before = line.substring(0, position.column);
                if (/^[\w.]$/.test(event.args) && /[A-Za-z0-9_\.\$]{3}\:?$/.test(before))
                    editor.execCommand('startAutocomplete');
                else if (event.args == ':' && /[A-Za-z0-9]\:\:\:?$/.test(before))
                    editor.execCommand('startAutocomplete');
            }
            else if (event.command.name == 'backspace' &&
                editor.completer &&
                editor.completer.activated)
                    editor.execCommand('startAutocomplete');
            else if (event.command.name === 'startAutocomplete')
                SuggestIcons.add(event.editor);
        });

        this.editor.commands.on('exec', (event) => {
            if (event.command.name === 'indent') {
                let editor = event.editor;
                let position = editor.getCursorPosition();
                let line = editor.getSession().getDocument().getLine(position.row);
                let before = line.substring(0, position.column);
                if (/[A-Za-z0-9_\.\$\:]$/.test(before)) {
                    editor.execCommand('startAutocomplete');
                    event.preventDefault();
                }
            }
        });

        // clear the default completers
        this.editor.completers.splice(0, this.editor.completers.length);

        // add the new one
        this.editor.completers.push(new Suggest(() => {
            return this.getColumnNames();
        }));

        this.getColumnNames = () => {
            return this.requestData('columns', {  })
				.then((data) => {
	                return data.columns.map(col => col.name);
	            }).then((names) => {
					// exclude filters
					let index = 0;
					for (;index < names.length; index++) {
						let name = names[index];
						if (/^Filter [1-9][0-9]*$/.exec(name) ||
							/^F[1-9][0-9]* \([1-9][0-9]*\)$/.exec(name))
								continue; // a filter
						else
							break; // not a filter
					}
					return names.slice(index);
				});
        };

        this.toggleMenu = (ui) => {
            if ( ! this.$menu.hasClass('visible'))
		        this.showMenu(ui);
		    else
		        this.hideMenu(ui);
        };

        this.showMenu = (ui) => {
		    this.$menu.addClass('visible');
        };

        this.hideMenu = (ui) => {
	        this.$menu.removeClass('visible');

	        ui.view.model.options.beginEdit();
	        ui.figWidth.setValue(this.$figWidth.val());
	        ui.figHeight.setValue(this.$figHeight.val());
	        ui.output.setValue(this.$output.val());
	        ui.R.setValue(this.$r.val());
	        ui.view.model.options.endEdit();
        };

        this.run = (ui) => {
            console.log(this.currentSession);

            let script = this.currentSession.getDocument().getValue();

            this.getColumnNames().then((columns) => {

                ui.view.model.options.beginEdit();

                ui.figWidth.setValue(this.$figWidth.val());
	            ui.figHeight.setValue(this.$figHeight.val());
	            ui.output.setValue(this.$output.val());
	            ui.R.setValue(this.$r.val());

                let match = script.match(/^\s*\#\s*\((.*)\)/);
                if (match !== null) {
                    let content = match[1];
                    let vars = content.split(',');
                    vars = vars.map(s => s.trim());
                    vars = vars.filter(v => columns.includes(v));
                    ui.vars.setValue(vars);
                    ui.code.setValue(script);
					this.currentSession.allColumns = false;
                }
                else {
                    ui.vars.setValue(columns);
                    ui.code.setValue(script);
					this.currentSession.allColumns = true;
                }

                // toggle toggle so the analysis *always* reruns
                // even if nothing has changed
                ui.toggle.setValue( ! ui.toggle.value());

                ui.view.model.options.endEdit();

                this.editor.focus();
            });
    	};

        this.$editor.on('keydown', (event) => {

            if (event.keyCode === 13 && (event.metaKey || event.ctrlKey) && event.shiftKey) {
                // ctrl+shift+enter
                this.run(ui);
                event.stopPropagation();
            }
            else if (event.keyCode === 65 && event.metaKey) {
                // ctrl+a
                this.$editor.select();
            }
            else if (event.keyCode === 67 && (event.metaKey || event.ctrlKey) && event.shiftKey)             {
                // ctrl+shift+c
                this.editor.toggleCommentLines();
            }
            else if (event.keyCode === 191 && (event.metaKey || event.ctrlKey)) {
                // ctrl+/
                this.editor.toggleCommentLines();
            }
        });


	},

	onDataChanged(ui, event) {
		if ( ! this.currentSession.allColumns)
			return;
		if (event.dataType !== 'columns')
			return;
		this.getColumnNames().then((columns) => {
			let old = ui.vars.value();
			if ( ! _.isEqual(old, columns))
				ui.vars.setValue(columns);
		});
	},

    update(ui, event) {

        let id = event.id;
        this.currentSession = this.editSessions[id];

        if (this.currentSession === undefined) {

            let code = ui.code.value();
            this.currentSession = ace.createEditSession(code, 'ace/mode/r');
            this.editSessions[id] = this.currentSession;
        }

        this.editor.setSession(this.currentSession);

        this.$figWidth.val(ui.figWidth.value());
        this.$figHeight.val(ui.figHeight.value());
        this.$output.val(ui.output.value());
        this.$r.val(ui.R.value());
    },
};


module.exports = events;
