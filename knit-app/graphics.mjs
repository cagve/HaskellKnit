const CELL_SIZE = 40;

function drawDebugCanvas(pattern) {
	const colors = getColors();
	const canvas = document.getElementById("patternCanvas");
	const ctx = canvas.getContext("2d");
	ctx.clearRect(0, 0, canvas.width, canvas.height);

	const compressedPattern = compressPattern(pattern);
	resizeCanvasToCompressedPattern(compressedPattern);
	drawCompressedPattern(ctx, compressedPattern, colors);
}

function compressPattern(pattern) {
	return pattern.map(row => {
		const compressedRow = [];
		let currentColor = row[0][1];
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

function drawCompressedPattern(ctx, compressedPattern, colors) {
	let y = 0;
	compressedPattern.forEach(compressedRow => {
		let x = 0;
		compressedRow.forEach(([colorValue, repetitions]) => {
			ctx.fillStyle = colors[colorValue] || '#000';
			ctx.fillRect(x * CELL_SIZE, y * CELL_SIZE, repetitions * CELL_SIZE, CELL_SIZE);
			ctx.strokeRect(x * CELL_SIZE, y * CELL_SIZE, repetitions * CELL_SIZE, CELL_SIZE);

			for (let i = 0; i <= repetitions; i++) {
				ctx.beginPath();
				ctx.moveTo((x + i) * CELL_SIZE, y * CELL_SIZE);
				ctx.lineTo((x + i) * CELL_SIZE, (y + 1) * CELL_SIZE);
				ctx.stroke();
			}

			if (repetitions > 1) {
				printText(ctx, repetitions, x, y, colors[colorValue]);
			}

			x += repetitions;
		});
		y++;
	});
}

function drawCanvas(pattern) {
	const colors = getColors();
	const canvas = document.getElementById("patternCanvas");
	const ctx = canvas.getContext("2d");
	ctx.clearRect(0, 0, canvas.width, canvas.height);

	resizeCanvasToPattern(pattern);

	const matrix = pattern.map(row => row.map(cell => parseInt(cell[1])));

	matrix.forEach((row, i) => {
		row.forEach((val, j) => {
			ctx.fillStyle = colors[val] || '#000';
			ctx.fillRect(j * CELL_SIZE, i * CELL_SIZE, CELL_SIZE, CELL_SIZE);
			ctx.strokeRect(j * CELL_SIZE, i * CELL_SIZE, CELL_SIZE, CELL_SIZE);
		});
	});
}

function getColors() {
	return Array.from(document.getElementsByName('colors')).map(input => input.value);
}

function printText(ctx, repetitions, x, y, color) {
	const centerX = (x + repetitions / 2) * CELL_SIZE;
	const centerY = (y + 0.5) * CELL_SIZE;
	const radius = CELL_SIZE * 0.4;

	ctx.beginPath();
	ctx.fillStyle = color;
	ctx.arc(centerX, centerY, radius, 0, Math.PI * 2);
	ctx.fill();
	ctx.strokeStyle = 'rgba(0, 0, 0, 0.5)';
	ctx.stroke();

	ctx.fillStyle = '#000';
	ctx.font = `${Math.max(10, CELL_SIZE / 2.5)}px Arial`;
	ctx.textAlign = 'center';
	ctx.textBaseline = 'middle';
	ctx.fillText(repetitions.toString(), centerX, centerY);
}

function resizeCanvasToPattern(pattern) {
	const canvas = document.getElementById("patternCanvas");
	const cols = pattern[0].length;
	const rows = pattern.length;
	canvas.width = cols * CELL_SIZE;
	canvas.height = rows * CELL_SIZE;
}

function resizeCanvasToCompressedPattern(compressedPattern) {
	const canvas = document.getElementById("patternCanvas");
	const rows = compressedPattern.length;
	const maxCols = Math.max(...compressedPattern.map(
		row => row.reduce((sum, [, count]) => sum + count, 0)
	));
	canvas.width = maxCols * CELL_SIZE;
	canvas.height = rows * CELL_SIZE;
}

export function drawImg(pattern) {
	const checkbox = document.getElementById('debugImg');
	checkbox.checked ? drawDebugCanvas(pattern) : drawCanvas(pattern);
}

export function isCanvasBlank() {
	const canvas = document.getElementById("patternCanvas");
	const ctx = canvas.getContext('2d');
	const pixelBuffer = new Uint32Array(
		ctx.getImageData(0, 0, canvas.width, canvas.height).data.buffer
	);
	return !pixelBuffer.some(color => color !== 0);
}

