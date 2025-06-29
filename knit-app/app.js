import {setCurrentRow, drawImg, setPattern, isCanvasBlank} from './graphics.mjs';

const API_URL = "http://127.0.0.1:8080";
const API_URL_PATTERNS = API_URL+"/patterns";
const DEFAULT_COLORS = ['#ffffff', '#B2EBF2', '#FFCCBC']

let APP_STATE = 0;
let currentRow = 0;


const colorMap = [
  '#ffffff', // C0 - blanco
  '#42a5f5', // C1 - azul claro
  '#ef6c00', // C2 - naranja
  '#66bb6a', // C3 - verde claro
  '#ab47bc', // C4 - morado
  '#ffa726', // C5 - naranja claro
  '#8d6e63', // C6 - marrón
  '#26c6da', // C7 - cian
  '#d4e157', // C8 - lima
  '#ff7043'  // C9 - rojo anaranjado
];

const shortRadio = document.getElementById("shortRadio");
const extendedRadio = document.getElementById("extendedRadio");
const imgRadio = document.getElementById("imgRadio");
const titleDOM = document.getElementById("patternTitle")
const explainExprDOM = document.getElementById("explainExprBox")
const explainClusterDOM = document.getElementById("explainClusterBox")
const fileInput = document.getElementById("fileInput");
const uploadForm = document.getElementById("uploadForm");
const imgBoxDOM = document.getElementById("imgBox")
const radioBoxesDOM = document.getElementById("radioBoxes")
const prevBtn = document.getElementById('prevRow');
const nextBtn = document.getElementById('nextRow');
const rowIndicator = document.getElementById('currentRowIndicator');
const exprInstructions = document.getElementById('patternExprInstructions');
const clusterInstructions = document.getElementById('patternClusterInstructions');
const emptyMsg = document.getElementById('emptyMsg');

const currentFiles = [];

function loadFiles(){
	fetch(API_URL_PATTERNS)
		.then(res => res.json())
		.then(files => {
			const list = document.getElementById("file-list");
			list.innerHTML = ""; // borrar mensaje de carga

			if (!Array.isArray(files) || files.length === 0) {
				list.innerHTML = "<li>No hay archivos disponibles.</li>";
				return;
			}

			files.forEach(file => {
				let li = createFileListItem(file)
				list.appendChild(li);
				currentFiles.push(file);
			});
			updatePattern(currentFiles[0])
		})
		.catch(err => {
			document.getElementById("file-list").innerHTML =
				`<li>❌ Error al cargar archivos: ${err.message}</li>`;
		});
}

function deleteFile(filename){
	fetch(API_URL+'/delete/'+filename, {
		method: "DELETE",
	})
		.then(res => res.json())
		.then(data => {
			alert(`${data.deleteMsg}`);
			loadFiles();
		})
		.catch(err => {
			alert("Error de conexión al eliminar el archivo: "+err);
		});
}

function uploadFile(file) {
	const formData = new FormData();
	formData.append("file", file);

	fetch(API_URL_PATTERNS, {
		method: "POST",
		body: formData
	})
		.then(res => res.json())
		.then(data => {
			alert(data.uploadMsg)
			loadFiles();
			fileInput.value ='';
		})
		.catch(err => {
			alert("Error de red al subir archivo.");
		});
};

function updatePattern(pattern){
	resetImg();

	currentRow = 0
	setCurrentRow(currentRow)
	shortRadio.checked  = "block"
	clusterInstructions.innerHTML = ''
	exprInstructions.innerHTML = ''
	fetch(API_URL_PATTERNS+"/"+pattern)
		.then(res => res.json())
		.then(data=>{
			setPattern(pattern);	
			let linesCompact = data.patternCompact
			titleDOM.innerHTML = "Pattern: " + pattern
			titleDOM.setAttribute("name", pattern)
			linesCompact.forEach((line, idx) => {
				if (!line){
					return
				}
				const li = document.createElement('li')
				li.addEventListener('click', () => {
					currentRow = idx;   // actualizar fila seleccionada
					setCurrentRow(currentRow)
					updateRow();    
				});
				clusterInstructions.appendChild(li)
				li.textContent = line.contents
			})

			let linesExpr = data.patternExplanation
			const elems = createInstructionElements(linesExpr);
			elems.forEach(el => exprInstructions.appendChild(el));


			if (data.colorPattern){
				const flatArray = data.patternStruct.instructions.flat();
				const uniqueElements = new Set(flatArray);
				const n = uniqueElements.size;
				updateImg(data.patternStruct.instructions, colorMap.slice(0,n));
			}
			APP_STATE = 0;
			updateAppState()
			updateRow()
		})
		.catch(err => {
			alert("Error de lectura"+ err);
		});

}


fileInput.onchange = function() {
    const file = fileInput.files[0];
    if (!file) {
      return;
    }
    uploadFile(file);
};



function updateRow(){
	let li_elements = ""
	if (APP_STATE === 2) {
		if (!isCanvasBlank()){
			 emptyMsg.style.display="none"
			 drawImg()
		}else {
			 emptyMsg.style.display="block"
		}
	} else{
		if (APP_STATE === 1) {
			li_elements = clusterInstructions.querySelectorAll('li');
		} else {
			li_elements = exprInstructions.querySelectorAll('li');
		}
		li_elements.forEach((li, i) => {
			li.classList.toggle('highlighted', i === currentRow);
		});

		const selectedLi = li_elements[currentRow];
		if (selectedLi) {
			selectedLi.scrollIntoView({
				behavior: 'smooth',
				block: 'center'
			});
		} 
	}

	rowIndicator.textContent = `Row ${currentRow + 1}`;
}

//
	prevBtn.onclick = () => {
		if (currentRow > 0) {
			currentRow--;
			setCurrentRow(currentRow)
			updateRow();
		}
	};

nextBtn.onclick = () => {
	let li_elements = ''
	
	if (APP_STATE === 1) {
		li_elements = clusterInstructions.querySelectorAll('li');
	} else {
		li_elements = exprInstructions.querySelectorAll('li');
	}

	if (currentRow < li_elements.length - 1) {
		currentRow++;
		setCurrentRow(currentRow)
		updateRow();
	}
};


function updateAppState() {
	explainExprDOM.style.display = APP_STATE === 0 ? "block" : "none";
	explainClusterDOM.style.display = APP_STATE === 1 ? "block" : "none";
	imgBoxDOM.style.display = APP_STATE === 2 ? "flex" : "none";
}

document.querySelectorAll('input[name="appState"]').forEach(radio => {
	radio.addEventListener('change', (e) => {
		APP_STATE = parseInt(e.target.value);
		updateAppState();
		updateRow();
	});
});

function updateImg(pattern, colors = colorMap){
	setPattern(pattern)
	document.getElementById("patternCanvas").style.display="block";
	let divColorPickerBox = document.createElement('div')
	divColorPickerBox.id = "colorPickerBox"
	let divNav = document.createElement('div')
	let debugCheckbox = document.createElement('input')
	debugCheckbox.id = "debugImg"
	debugCheckbox.type="checkbox"
	debugCheckbox.onchange = () => drawImg()
	divNav.id = "imgNav"
	for (let i = 0; i < colors.length; i++){
		let inputColor = document.createElement('input')
		inputColor.type = "color"
		inputColor.name = "colors"
		inputColor.id = "color"+1
		inputColor.onchange = () => drawImg()
		inputColor.value = (i>colors.length) ? '#ffffff' : colors[i];
		divNav.appendChild(inputColor)
	}
	divNav.appendChild(debugCheckbox)
	imgBoxDOM.prepend(divNav)

	// CHECKBOX Tex
	const text = document.createTextNode(" Magic marker"); 
	debugCheckbox.parentNode?.insertBefore(text, debugCheckbox.nextSibling);
	drawImg()
}



function createFileListItem(file) {
	// Crear <li>
		const li = document.createElement("li");

	// Crear <span> con el nombre del archivo
	const span = document.createElement("span");
	span.textContent = file;

	// Crear contenedor para los botones
	const actionsDiv = document.createElement("div");
	actionsDiv.className = "file-actions";

	// Btn Open
	const openButton = document.createElement("button");
	openButton.textContent = "Open";
	openButton.onclick = () => updatePattern(file)

	// Btn Close
	const deleteButton = document.createElement("button");
	deleteButton.textContent = "Delete";
	deleteButton.onclick = () => deleteFile(file)

	actionsDiv.appendChild(openButton);
	actionsDiv.appendChild(deleteButton);

	li.appendChild(span);
	li.appendChild(actionsDiv);

	return li;
}



function resetImg(){
	const canvas = document.getElementById("patternCanvas");
	const ctx = canvas.getContext("2d");
	ctx.clearRect(0, 0, canvas.width, canvas.height);

	const elem = document.getElementById('imgNav');
	if (elem) {
		elem.remove();
	}
}

function createInstructionElements(linesExpr) {
	const elements = [];

	linesExpr.forEach((item, idx) => {
		if (!item) return;

		if (item.tag === "Line") {
			const li = document.createElement('li');
			li.textContent = item.contents;
			li.addEventListener('click', () => {
				currentRow = idx;
				updateRow();
			});
			elements.push(li);

		} else if (item.tag === "Block") {
			const blockContainer = document.createElement('li');
			blockContainer.style.cursor = 'pointer';

			const header = document.createElement('div');
			header.style.display = 'flex';
			header.style.justifyContent = 'space-between';
			header.style.alignItems = 'center';

			const title = document.createElement('span');
			title.textContent = item.contents[0];

			const toggleIcon = document.createElement('span');
			toggleIcon.textContent = '▶';
			toggleIcon.style.marginLeft = '8px';
			toggleIcon.style.fontSize = '0.9em';

			header.appendChild(title);
			header.appendChild(toggleIcon);
			blockContainer.appendChild(header);

			const subList = document.createElement('ul');
			subList.style.display = 'none';
			subList.style.marginTop = '5px';
			blockContainer.appendChild(subList);

			item.contents.slice(1).forEach((subItem) => {
				const subLi = document.createElement('li');
				subLi.textContent = subItem;
				subList.appendChild(subLi);
			});

			header.addEventListener('click', (e) => {
				e.stopPropagation(); // evitar conflictos si hay eventos padres
				const isVisible = subList.style.display === 'block';
				subList.style.display = isVisible ? 'none' : 'block';
				toggleIcon.textContent = isVisible ? '▶' : '▼';
			});

			elements.push(blockContainer);
		}
	});

	return elements;
}


loadFiles();
