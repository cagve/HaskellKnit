const CELL_SIZE = 40
let cellSize = CELL_SIZE
// let ZOOM_FACTOR = 1;

function drawDebugCanvas(pattern, scale = 1) {
    const colors = getColors();
    const canvas = document.getElementById("patternCanvas");
    const ctx = canvas.getContext("2d");
    ctx.clearRect(0, 0, canvas.width, canvas.height);
    const compressedPattern = compressPattern(pattern);
    drawCompressedPattern(ctx, compressedPattern, colors, scale);
}

function compressPattern(pattern) {
    return pattern.map(row => {
        const compressedRow = [];
        let currentColor = row[0][1]; // Asumiendo que cada celda es [x, colorValue]
        let count = 1;
        
        for (let i = 1; i < row.length; i++) {
            if (row[i][1] === currentColor) {
                count++;
            } else {
                compressedRow.push([currentColor, count]);
                currentColor = row[i][1];
                count = 1;
            }
        }
        compressedRow.push([currentColor, count]);
        return compressedRow;
    });
}

function drawCompressedPattern(ctx, compressedPattern, colors, scale=1) {
	if (scale == null){
		cellSize = 40;
	}else{
		cellSize = cellSize *= scale
	}
    let y = 0; 
    compressedPattern.forEach(compressedRow => {
        let x = 0; 
        
        compressedRow.forEach(([colorValue, repetitions]) => {
            ctx.fillStyle = colors[colorValue] || '#000';
            ctx.fillRect(x * cellSize, y * cellSize, repetitions * cellSize, cellSize);
            ctx.strokeRect(x * cellSize, y * cellSize, repetitions * cellSize, cellSize);

			 for (let i = 0; i <= repetitions; i++) {
                ctx.beginPath();
                ctx.moveTo((x + i) * cellSize, y * cellSize);
                ctx.lineTo((x + i) * cellSize, (y + 1) * cellSize);
                ctx.stroke();
            }

            if (repetitions > 1) {
				printText(ctx, repetitions, x, y, colors[colorValue])
            }
            
            x += repetitions;
        });
        
        y++;
    });
}

function drawCanvas(pattern, scale = ZOOM_FACTOR) {
	if (scale == null){
		cellSize = 40;
	}else{
		cellSize = cellSize *= scale
	}
	let colors = getColors()
	const canvas = document.getElementById("patternCanvas");
	const ctx = canvas.getContext("2d");
    ctx.clearRect(0, 0, canvas.width, canvas.height);
	resizeCanvasToPattern(pattern)

	const rows = pattern
	const matrix = rows.map(row => row.map(cell => parseInt(cell[1])));

	matrix.forEach((row, i) => {
		row.forEach((val, j) => {
			ctx.fillStyle = colors[val] || '#000';
			ctx.fillRect(j * cellSize, i * cellSize, cellSize, cellSize);
			ctx.strokeRect(j * cellSize, i * cellSize, cellSize, cellSize); // opcional: borde
		});
	});
}


function getColors(){
	let colors =  Array.from(document.getElementsByName('colors'))
		.map(input => input.value)
	return colors
}

function printText(ctx, repetitions, x, y, color){
	const centerX = (x + repetitions/2) * cellSize;
	const centerY = (y + 0.5) * cellSize;
	const radius = cellSize * 0.4; // Radio del círculo

	// Dibujar círculo blanco
	ctx.beginPath();
	ctx.fillStyle = color;
	ctx.arc(centerX, centerY, radius, 0, Math.PI * 2);
	ctx.fill();
	ctx.strokeStyle = 'rgba(0, 0, 0, 0.5)';
	ctx.stroke();

	// Dibujar texto
	ctx.fillStyle = '#000';
	ctx.font = `${Math.max(10, cellSize/2.5)}px Arial`;
	ctx.textAlign = 'center';
	ctx.textBaseline = 'middle';
	ctx.fillText(repetitions.toString(), centerX, centerY);
}


function resizeCanvasToPattern(pattern) {
    const canvas = document.getElementById("patternCanvas");
    const cols = pattern[0].length;
    const rows = pattern.length;
    
    canvas.width = cols * cellSize;
    canvas.height = rows * cellSize;
}


export function zoomIn(pattern) {
    drawImg(pattern, 1.1); 
}

export function zoomOut(pattern) {
    drawImg(pattern, 0.9); 
}

export function zoomReset(pattern) {
    drawImg(pattern, null);
}

export function drawImg(pattern, scale = 1){
	const checkbox = document.getElementById('debugImg');
	checkbox.checked ? drawDebugCanvas(pattern, scale) : drawCanvas(pattern, scale);
}



export function isCanvasBlank() {
	const canvas = document.getElementById("patternCanvas");
	const ctx = canvas.getContext('2d');
	const pixelBuffer = new Uint32Array(
		ctx.getImageData(0, 0, canvas.width, canvas.height).data.buffer
	);
	return !pixelBuffer.some(color => color !== 0);
}
