const workspaceDiv = document.getElementById('workspace');

getTest().then(x => { console.log(x); workspaceDiv.textContent = JSON.stringify(x.data) }).catch(e => console.log(e));
