const API_URL = "http://127.0.0.1:8080";
const API_URL_PATTERNS = API_URL+"/patterns";
fetch(API_URL_PATTERNS+'/topsol.knit')
	.then(response => response.json())
	.then(data => {
		console.log("HOLA MUNDO")
		const filename = data.filename || 'archivo desconocido';
		document.getElementById('output').textContent = `patron: ${filename}`;
	})
	.catch(error => {
		document.getElementById('output').textContent = 'Error al cargar el patrón';
		console.error('Error:', error);
	});
