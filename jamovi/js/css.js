
const css = `
#editor-box {
    margin-bottom: 6px ;
    flex-grow: 1 ;
    display: flex ;
    flex-direction: column ;
    position: relative ;
    height:500px;
}

#editor {
    border: 1px solid #AAAAAA ;
    flex-grow: 1 ;
}

#toolbar {
    display: flex ;
    position: absolute ;
    top: 4px ;
    right: 24px ;
    z-index: 10 ;
}

#info {
    display: flex ;
    position: absolute ;
    bottom: 4px ;
    right: 16px ;
    color: #333333 ;
}

#run, #config {
    width: 21px ;
    height: 21px ;
    border-radius: 2px ;
    background-size: auto 80% ;
    background-position: center ;
    background-repeat: no-repeat ;
}

#run:hover, #config:hover {
    background-color: #EEEEEE ;
}

#menu {
    opacity: 0 ;
    pointer-events: none ;
    transition: .1s all ;

    display: grid ;
    position: absolute ;
    top: 24px ;
    right: 0px ;
    color: #333333 ;
    background-color: #f4f4f4 ;
    border: 1px solid #C1C1C1 ;
    box-shadow: 0px 0px 5px #C1C1C1 ;
    border-radius: 2px ;
    z-index: 20 ;

    padding: 12px ;
    align-items: center ;
    grid-column-gap: 6px ;
    grid-row-gap: 12px ;
}

#menu.visible {
    opacity: 1 ;
    pointer-events: auto ;
}

#menu > label {
    grid-column: 1 ;
    white-space: nowrap ;
}

#output-label, #output {
    grid-row: 1 ;
}

#output {
    grid-column: 2 / span 2 ;
}

#r-label, #r-version {
    grid-row: 3 ;
}

#figure-size {
    grid-row: 2;
    grid-column: 1 / span 2 ;
    display: grid ;
    justify-content: left ;
    align-items: baseline ;
    grid-gap: 4px ;
    margin: 10px 0px;
}

#figure-size-title {
    grid-column: 1 / span 2;
    margin-bottom: 8px;
    font-weight: bold;
}

#figure-size label {
    padding-left: 12px ;
    grid-column: 1 ;
}

#figure-size input {
    width: 50px;
    grid-column: 2 ;
}

#run {
    background-image: url('data:image/svg+xml;base64,PHN2ZyBpZD0ic3ZnMiIgeG1sbnM6cmRmPSJodHRwOi8vd3d3LnczLm9yZy8xOTk5LzAyLzIyLXJkZi1zeW50YXgtbnMjIiB4bWxucz0iaHR0cDovL3d3dy53My5vcmcvMjAwMC9zdmciIGhlaWdodD0iMjQiIHdpZHRoPSIyNCIgdmVyc2lvbj0iMS4xIiB4bWxuczpjYz0iaHR0cDovL2NyZWF0aXZlY29tbW9ucy5vcmcvbnMjIiB4bWxuczpkYz0iaHR0cDovL3B1cmwub3JnL2RjL2VsZW1lbnRzLzEuMS8iIHZpZXdCb3g9IjAgMCAyNCAyNCI+PG1ldGFkYXRhIGlkPSJtZXRhZGF0YTEwIj48cmRmOlJERj48Y2M6V29yayByZGY6YWJvdXQ9IiI+PGRjOmZvcm1hdD5pbWFnZS9zdmcreG1sPC9kYzpmb3JtYXQ+PGRjOnR5cGUgcmRmOnJlc291cmNlPSJodHRwOi8vcHVybC5vcmcvZGMvZGNtaXR5cGUvU3RpbGxJbWFnZSIvPjxkYzp0aXRsZS8+PC9jYzpXb3JrPjwvcmRmOlJERj48L21ldGFkYXRhPjxwYXRoIGlkPSJwYXRoNCIgZmlsbD0iIzJhOGEyYSIgZD0ibTMgMjJ2LTIwbDE4IDEwLTE4IDEweiIvPjwvc3ZnPg==');
}

#config {
    background-image: url('data:image/svg+xml;base64,PHN2ZyBpZD0ic3ZnMiIgeG1sbnM6cmRmPSJodHRwOi8vd3d3LnczLm9yZy8xOTk5LzAyLzIyLXJkZi1zeW50YXgtbnMjIiB4bWxucz0iaHR0cDovL3d3dy53My5vcmcvMjAwMC9zdmciIGhlaWdodD0iMjQiIHdpZHRoPSIyNCIgdmVyc2lvbj0iMS4xIiB4bWxuczpjYz0iaHR0cDovL2NyZWF0aXZlY29tbW9ucy5vcmcvbnMjIiB4bWxuczpkYz0iaHR0cDovL3B1cmwub3JnL2RjL2VsZW1lbnRzLzEuMS8iIHZpZXdCb3g9IjAgMCAyNCAyNCI+PG1ldGFkYXRhIGlkPSJtZXRhZGF0YTEwIj48cmRmOlJERj48Y2M6V29yayByZGY6YWJvdXQ9IiI+PGRjOmZvcm1hdD5pbWFnZS9zdmcreG1sPC9kYzpmb3JtYXQ+PGRjOnR5cGUgcmRmOnJlc291cmNlPSJodHRwOi8vcHVybC5vcmcvZGMvZGNtaXR5cGUvU3RpbGxJbWFnZSIvPjxkYzp0aXRsZS8+PC9jYzpXb3JrPjwvcmRmOlJERj48L21ldGFkYXRhPjxwYXRoIGlkPSJwYXRoNDEzOCIgZD0ibTEwLjk0OSAwLjgxNDQ1Yy0wLjE4NCAwLjU1MTk1LTAuMzQ1IDEuMS0wLjU5MiAxLjU4Mi0wLjMzIDAuNjQ3LTAuODc3OSAxLjI0MTgtMS42NzkzIDEuNTc0M2EwLjgxNDggMC44MTQ4IDAgMCAxIC0wLjAwMTk1IDAgMC44MTQ4IDAuODE0OCAwIDAgMSAtMC4yOTY4OCAwLjA2MjVjLTAuNzAxNCAwLjIxNDItMS40MDAzIDAuMjExMS0yLjAwNTkgMC4wMTU2LTAuNTE1OC0wLjE2NjQtMS4wMTg1LTAuNDM5NC0xLjU0MS0wLjcwMTFsLTEuNDg0NCAxLjQ4NDRjMC4yNjA3IDAuNTIwMiAwLjUzMjYgMS4wMjEzIDAuNjk5MyAxLjUzNyAwLjIyMzcgMC42OTIyIDAuMjU2OCAxLjUwMzgtMC4wNzYyIDIuMzA2Ny0wLjMzMjQgMC44MDE3LTAuOTI3MiAxLjM1MDItMS41NzQyIDEuNjgxMi0wLjQ4MjEgMC4yNDctMS4wMjk5IDAuNDA4LTEuNTgyIDAuNTkydjIuMTAxNmMwLjU1MTMxIDAuMTgzMzEgMS4wOTggMC4zNDMxOSAxLjU4MDEgMC41ODk4NCAwLjY0NzI1IDAuMzMxMTYgMS4yNDM1IDAuODc5OTMgMS41NzYyIDEuNjgxNmEwLjgxNDggMC44MTQ4IDAgMCAxIDAgMC4wMDJjMC4zMzQ4MiAwLjgwOTg4IDAuMjk3OCAxLjYyNDUgMC4wNzIyNjYgMi4zMTg0LTAuMTY3NjggMC41MTU4Ny0wLjQzNzcxIDEuMDEwOC0wLjY5NTMxIDEuNTIzNGwxLjQ4NDQgMS40ODYzYzAuNTE5MDgtMC4yNjAyMSAxLjAxOTItMC41MzI0MSAxLjUzNTItMC42OTkyMiAwLjYwNjEtMC4xOTU5NCAxLjMwNjYtMC4yMDAwNSAyLjAwOTggMC4wMTM2N2EwLjgxNDggMC44MTQ4IDAgMCAxIDAuMjk4ODMgMC4wNjI1IDAuODE0OCAwLjgxNDggMCAwIDEgMC4wMDE5NSAwYzAuODAwOSAwLjMzMTcxIDEuMzQ4NyAwLjkyNTU2IDEuNjc5NyAxLjU3MjMgMC4yNDY2NyAwLjQ4MTk4IDAuNDA3NzcgMS4wMzA2IDAuNTkxOCAxLjU4NGgyLjEwMTZjMC4xODM5NC0wLjU1MjQzIDAuMzQ0MTUtMS4wOTk2IDAuNTkxOC0xLjU4MiAwLjMzMjM3LTAuNjQ3NDcgMC44ODQzMS0xLjI0MzUgMS42ODc1LTEuNTc4MWEwLjgxNDggMC44MTQ4IDAgMCAxIDAuMDAyIDAgMC44MTQ4IDAuODE0OCAwIDAgMSAwLjI5ODgzIC0wLjA2MjVjMC42OTczNi0wLjIxMTcxIDEuMzkzMy0wLjIwNzIxIDEuOTk2MS0wLjAxMTcyIDAuNTEzOTkgMC4xNjY3IDEuMDE2NSAwLjQzOTQ1IDEuNTM5MSAwLjcwMTE3bDEuNDg0NC0xLjQ4NDRjLTAuMjYwNC0wLjUxOTQ0LTAuNTMyNjItMS4wMTkyLTAuNjk5MjItMS41MzUyLTAuMjIzNjYtMC42OTI2My0wLjI1NjM4LTEuNTA1MyAwLjA3NjE3LTIuMzA4NiAwLjMzMjY2LTAuODAyMzEgMC45Mjg1OS0xLjM1MDIgMS41NzYyLTEuNjgxNiAwLjQ4MjUtMC4yNDY5MiAxLjAzMDItMC40MDgxNCAxLjU4Mi0wLjU5MTh2LTIuMTAxNmMtMC41NTctMC4xODQtMS4xMDktMC4zNDYtMS41OS0wLjU5My0wLjY0Ni0wLjMzMS0xLjIzNi0wLjg3OTEtMS41NjctMS42NzczYTAuODE0OCAwLjgxNDggMCAwIDEgMCAtMC4wMDE5NSAwLjgxNDggMC44MTQ4IDAgMCAxIC0wLjA2MjUgLTAuMjk2ODhjLTAuMjEzLTAuNzAxNy0wLjIxLTEuNDAwMi0wLjAxNS0yLjAwNTkgMC4xNjctMC41MTU3IDAuNDQtMS4wMTkgMC43MDEtMS41NDFsLTEuNDg0LTEuNDg0M2MtMC41MTkgMC4yNTk5LTEuMDE5IDAuNTMyMy0xLjUzNSAwLjY5OTItMC42MDYgMC4xOTYxLTEuMzA2IDAuMjAwMS0yLjAxLTAuMDEzN2EwLjgxNDggMC44MTQ4IDAgMCAxIC0wLjI5ODgzIC0wLjA2MjUgMC44MTQ4IDAuODE0OCAwIDAgMSAtMC4wMDIgMGMtMC44LTAuMzMxNi0xLjM0OC0wLjkyNC0xLjY3OS0xLjU3MDMtMC4yNDctMC40ODE5LTAuNDA4LTEuMDMxNy0wLjU5Mi0xLjU4NmgtMi4xMDE2em0xLjA1MSA2LjM3MWMyLjY0OTQgMCA0LjgxNDUgMi4xNjUxIDQuODE0NSA0LjgxNDVzLTIuMTY1MSA0LjgxNDUtNC44MTQ1IDQuODE0NS00LjgxNDUtMi4xNjUtNC44MTQ1LTQuODE0YzAtMi42NDk0IDIuMTY1MS00LjgxNDUgNC44MTQ1LTQuODE0NXoiIHRyYW5zZm9ybT0ibWF0cml4KDEuMDI2OSAwIDAgMS4wMjc1IC0uMzIyOTEgLS4zMzczMikiIHN0cm9rZT0iI2JlYmVkNiIgZmlsbD0iI2QzZDZlMyIvPjwvc3ZnPg==');

    margin-right: 3px;
}

.ace_editor.ace_autocomplete .ace_completion-highlight {
    color: inherit ;
    text-shadow: none ;
}

.ace_editor.ace_autocomplete .ace_line {
    line-height: 20px ;
}

.ace_icon_data,
.ace_icon_func,
.ace_icon_var,
.ace_icon_ns {
    display: inline-block ;
    width: 24px ;
    background-repeat: no-repeat ;
    background-size: 12px ;
    background-position: center ;
}

.ace_icon_data {
    background-image: url('data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAABYAAAAWCAYAAADEtGw7AAAAQklEQVR42mOImrXnPy0wA4jYeu8jVfGowZgG0yTy2jqn/8cFLly4SpYcyMwhajBIETXxEA6K0TAeDePRMMZjMC0wAFMtrQuuiigOAAAAAElFTkSuQmCC');
}

.ace_icon_func {
    background-image: url('data:image/svg+xml;base64,PHN2ZyBpZD0ic3ZnMiIgeG1sbnM6cmRmPSJodHRwOi8vd3d3LnczLm9yZy8xOTk5LzAyLzIyLXJkZi1zeW50YXgtbnMjIiB4bWxucz0iaHR0cDovL3d3dy53My5vcmcvMjAwMC9zdmciIGhlaWdodD0iMjQiIHdpZHRoPSIyNCIgdmVyc2lvbj0iMS4xIiB4bWxuczpjYz0iaHR0cDovL2NyZWF0aXZlY29tbW9ucy5vcmcvbnMjIiB4bWxuczpkYz0iaHR0cDovL3B1cmwub3JnL2RjL2VsZW1lbnRzLzEuMS8iIHZpZXdCb3g9IjAgMCAyNCAyNCI+PG1ldGFkYXRhIGlkPSJtZXRhZGF0YTEwIj48cmRmOlJERj48Y2M6V29yayByZGY6YWJvdXQ9IiI+PGRjOmZvcm1hdD5pbWFnZS9zdmcreG1sPC9kYzpmb3JtYXQ+PGRjOnR5cGUgcmRmOnJlc291cmNlPSJodHRwOi8vcHVybC5vcmcvZGMvZGNtaXR5cGUvU3RpbGxJbWFnZSIvPjxkYzp0aXRsZS8+PC9jYzpXb3JrPjwvcmRmOlJERj48L21ldGFkYXRhPjxyZWN0IGlkPSJyZWN0NDE1NiIgc3Ryb2tlLWxpbmVqb2luPSJyb3VuZCIgdHJhbnNmb3JtPSJtYXRyaXgoLjY4ODgxIC43MjQ5NCAtLjcxNjU4IC42OTc1MSAwIDApIiBoZWlnaHQ9IjIxLjYxMSIgd2lkdGg9IjExLjIxNSIgc3Ryb2tlPSIjM2M2Y2E1IiB5PSItMTEuMjQiIHg9IjExLjM2MiIgc3Ryb2tlLXdpZHRoPSIuNzk0NjEiIGZpbGw9IiM3NzlmY2UiLz48L3N2Zz4=');
}

.ace_icon_ns {
    background-image: url('data:image/svg+xml;base64,PHN2ZyBpZD0ic3ZnMiIgeG1sbnM6cmRmPSJodHRwOi8vd3d3LnczLm9yZy8xOTk5LzAyLzIyLXJkZi1zeW50YXgtbnMjIiB4bWxucz0iaHR0cDovL3d3dy53My5vcmcvMjAwMC9zdmciIGhlaWdodD0iMjQiIHdpZHRoPSIyNCIgdmVyc2lvbj0iMS4xIiB4bWxuczpjYz0iaHR0cDovL2NyZWF0aXZlY29tbW9ucy5vcmcvbnMjIiB4bWxuczpkYz0iaHR0cDovL3B1cmwub3JnL2RjL2VsZW1lbnRzLzEuMS8iIHZpZXdCb3g9IjAgMCAyNCAyNCI+PG1ldGFkYXRhIGlkPSJtZXRhZGF0YTEwIj48cmRmOlJERj48Y2M6V29yayByZGY6YWJvdXQ9IiI+PGRjOmZvcm1hdD5pbWFnZS9zdmcreG1sPC9kYzpmb3JtYXQ+PGRjOnR5cGUgcmRmOnJlc291cmNlPSJodHRwOi8vcHVybC5vcmcvZGMvZGNtaXR5cGUvU3RpbGxJbWFnZSIvPjxkYzp0aXRsZS8+PC9jYzpXb3JrPjwvcmRmOlJERj48L21ldGFkYXRhPjxwYXRoIGlkPSJwYXRoNDE1NCIgZD0ibTExLjk5OCAwLjYwOTM4LTEwLjQ2MyA1LjcwN3YxMS40OTRsMTAuNDY1IDUuNTg1IDEwLjQ2NS01LjU4NHYtMTEuNDNsLTEwLjQ2Ny01Ljc3MTZ6bS0wLjAwMzkgMS4xMzY3YTAuNTM0NCAwLjUzNDQgMCAwIDEgMC4yNTggMC4wNjY0bDcuOTE2IDQuMzY1MmEwLjUzNDQgMC41MzQ0IDAgMCAxIC0wLjAwNTkgMC45Mzk0NWwtNy45MDQgNC4yMTY4YTAuNTM0NCAwLjUzNDQgMCAwIDEgLTAuNTA4IC0wLjAwMmwtNy44NzMtNC4yOTI5YTAuNTM0NCAwLjUzNDQgMCAwIDEgMCAtMC45Mzc1bDcuODYxLTQuMjg5MWEwLjUzNDQgMC41MzQ0IDAgMCAxIDAuMjU1ODYgLTAuMDY2NDA2em05LjAxMTcgNi4wNDg4YTAuNTM0NCAwLjUzNDQgMCAwIDEgMC41MjkzIDAuNTM1MTZ2OC42MDE2YTAuNTM0NCAwLjUzNDQgMCAwIDEgLTAuMjgzMiAwLjQ3MDdsLTggNC4yNjc2YTAuNTM0NCAwLjUzNDQgMCAwIDEgLTAuNzg3MTEgLTAuNDcwN3YtOC41OTk2YTAuNTM0NCAwLjUzNDQgMCAwIDEgMC4yODMyIC0wLjQ3MDdsOC00LjI2OTVhMC41MzQ0IDAuNTM0NCAwIDAgMSAwLjI1NzgxIC0wLjA2NDQ1M3oiIHRyYW5zZm9ybT0ibWF0cml4KDEuMDU3MyAwIDAgMS4wNTI3IC0uNjg4MjAgLS42MjY0NikiIGZpbGw9IiM4OTViYTgiLz48L3N2Zz4=');
}

`;

let node = document.createElement('style');
node.innerHTML = css;
document.body.appendChild(node);

module.exports = undefined;
