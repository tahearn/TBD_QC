import './style.css'


function ui(divID) {
    let divUI = divID ? document.getElementById(divID) : document.createElement('div');

    divUI.innerHTML = `
<h1 class="text-3xl font-bold underline">Hello world!</h1>
    `;
}


export { ui };