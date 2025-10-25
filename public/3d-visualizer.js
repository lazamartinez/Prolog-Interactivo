// Reemplaza la inicialización al final del archivo:
document.addEventListener('DOMContentLoaded', function () {
    console.log('🎯 Inicializando sistema 3D...');

    // Inicializar visualizador inmediatamente
    initialize3DVisualizer();

    // Configurar auto-visualización después de un breve delay
    setTimeout(() => {
        setup3DAutoVisualization();

        // Verificar si ya hay datos cargados
        if (appState.currentData && appState.currentData.length > 0) {
            console.log('📊 Datos existentes encontrados, visualizando...');
            setTimeout(() => {
                visualizeDataIn3D(appState.currentData);
            }, 1000);
        }
    }, 1000);
});

// Función global para inicialización desde HTML
window.init3DVisualization = function (data, headers) {
    console.log('🎨 Inicializando visualización 3D desde ventana global');
    visualizeDataIn3D(data, headers);
};

// Sistema de Visualización 3D para TODOS los registros
class Prolog3DVisualizer {
    constructor(containerId) {
        this.container = document.getElementById(containerId);
        this.scene = null;
        this.camera = null;
        this.renderer = null;
        this.controls = null;
        this.objects = [];
        this.labels = []; // Array separado para etiquetas
        this.animationId = null;
        this.data = [];
        this.headers = [];

        this.init();
    }

    init() {
        try {
            console.log('🚀 Inicializando visualizador 3D...');

            // Verificar que Three.js está disponible
            if (typeof THREE === 'undefined') {
                throw new Error('Three.js no está cargado');
            }

            // Configuración de la escena
            this.scene = new THREE.Scene();
            this.scene.background = new THREE.Color(0x0f172a);

            // Cámara
            this.camera = new THREE.PerspectiveCamera(
                75,
                this.container.clientWidth / this.container.clientHeight,
                0.1,
                1000
            );
            this.camera.position.set(0, 5, 10);

            // Renderer
            const canvas = document.getElementById('3dCanvas');
            if (!canvas) {
                throw new Error('Canvas 3D no encontrado');
            }

            this.renderer = new THREE.WebGLRenderer({
                canvas: canvas,
                antialias: true,
                alpha: true
            });
            this.renderer.setSize(this.container.clientWidth, this.container.clientHeight);
            this.renderer.setPixelRatio(window.devicePixelRatio);

            // Iluminación
            this.setupLighting();

            // Controles
            this.setupControls();

            // Ejes de referencia
            this.addReferenceAxes();

            // Manejar redimensionamiento
            this.setupResizeHandler();

            // Iniciar animación
            this.animate();

            console.log('✅ Visualizador 3D inicializado correctamente');

        } catch (error) {
            console.error('❌ Error inicializando visualizador 3D:', error);
            this.showErrorMessage('Error inicializando visualizador 3D: ' + error.message);
        }
    }

    setupLighting() {
        // Luz ambiental
        const ambientLight = new THREE.AmbientLight(0x404040, 0.6);
        this.scene.add(ambientLight);

        // Luz direccional principal
        const directionalLight1 = new THREE.DirectionalLight(0xffffff, 0.8);
        directionalLight1.position.set(1, 1, 1);
        this.scene.add(directionalLight1);

        // Luz direccional secundaria
        const directionalLight2 = new THREE.DirectionalLight(0xffffff, 0.4);
        directionalLight2.position.set(-1, -1, -1);
        this.scene.add(directionalLight2);

        // Luz puntual
        const pointLight = new THREE.PointLight(0xffffff, 0.5, 100);
        pointLight.position.set(5, 5, 5);
        this.scene.add(pointLight);
    }

    setupControls() {
        if (typeof THREE.OrbitControls !== 'undefined') {
            this.controls = new THREE.OrbitControls(this.camera, this.renderer.domElement);
            this.controls.enableDamping = true;
            this.controls.dampingFactor = 0.05;
            this.controls.screenSpacePanning = false;
            this.controls.minDistance = 1;
            this.controls.maxDistance = 100;
            this.controls.maxPolarAngle = Math.PI;
        } else {
            console.warn('OrbitControls no disponible');
        }
    }

    setupResizeHandler() {
        const resizeObserver = new ResizeObserver(() => {
            this.onWindowResize();
        });
        resizeObserver.observe(this.container);
    }

    addReferenceAxes() {
        // Ejes de referencia (X, Y, Z)
        const axesHelper = new THREE.AxesHelper(5);
        this.scene.add(axesHelper);

        // Grid helper
        const gridHelper = new THREE.GridHelper(10, 10);
        gridHelper.rotation.x = Math.PI / 2;
        this.scene.add(gridHelper);
    }

    visualizeAllData(data, headers = []) {
        console.log('📊 Visualizando datos en 3D:', data?.length, 'registros');

        if (!data || !Array.isArray(data) || data.length === 0) {
            console.error('❌ Datos inválidos para visualización 3D');
            this.showNoDataMessage();
            return;
        }

        this.data = data;
        this.headers = headers || [];

        try {
            this.clearScene();
            this.clearLabels(); // Limpiar etiquetas anteriores
            this.addReferenceAxes();

            this.hideNoDataMessage();
            this.showLoading();

            // Procesar en el siguiente frame para no bloquear la UI
            requestAnimationFrame(() => {
                try {
                    this.processDataFor3D();
                    this.updateStats();
                    this.updateLegend();
                    this.hideLoading();

                    console.log(`✅ Visualización 3D completada: ${this.objects.length} objetos`);

                } catch (error) {
                    console.error('❌ Error en procesamiento 3D:', error);
                    this.hideLoading();
                    this.showErrorMessage(`Error al generar visualización: ${error.message}`);
                }
            });

        } catch (error) {
            console.error('❌ Error crítico en visualización 3D:', error);
            this.hideLoading();
            this.showErrorMessage('Error crítico en visualizador 3D');
        }
    }

    processDataFor3D() {
        const visualizationType = document.getElementById('visualizationType')?.value || 'scatter';

        switch (visualizationType) {
            case 'scatter':
                this.createScatterPlot();
                break;
            case 'network':
                this.createNetworkGraph();
                break;
            case 'timeline':
                this.createTimeline();
                break;
            case 'hierarchy':
                this.createHierarchy();
                break;
            case 'cube':
                this.createCubeVisualization();
                break;
            default:
                this.createScatterPlot();
        }
    }

    createScatterPlot() {
        const totalPoints = this.data.length;

        // 🔥 OPTIMIZACIÓN: Limitar puntos para datasets grandes
        let pointsToRender = totalPoints;
        let samplingRate = 1;

        if (totalPoints > 1000) {
            samplingRate = Math.ceil(totalPoints / 1000);
            pointsToRender = Math.floor(totalPoints / samplingRate);
            console.log(`🎯 Optimizando: mostrando ${pointsToRender} de ${totalPoints} puntos (rate: ${samplingRate})`);
        }

        const radius = Math.min(10, 5 + (pointsToRender / 1000));

        for (let i = 0; i < totalPoints; i += samplingRate) {
            const record = this.data[i];
            const effectiveIndex = i / samplingRate;

            // Calcular posición optimizada
            const angle = (effectiveIndex / pointsToRender) * Math.PI * 2 * 3;
            const distance = (effectiveIndex / pointsToRender) * radius;

            const x = Math.cos(angle) * distance;
            const y = Math.sin(angle) * distance;
            const z = (effectiveIndex / pointsToRender - 0.5) * radius * 2;

            // Geometría más simple para mejor rendimiento
            const geometry = new THREE.SphereGeometry(0.08, 6, 6); // Menos segmentos
            const material = new THREE.MeshPhongMaterial({
                color: this.getColorForRecord(record, i),
                transparent: true,
                opacity: 0.7
            });

            const sphere = new THREE.Mesh(geometry, material);
            sphere.position.set(x, y, z);
            sphere.userData = { record, index: i, originalIndex: i };

            this.scene.add(sphere);
            this.objects.push(sphere);

            // Solo agregar etiquetas si hay pocos puntos
            if (pointsToRender <= 100) {
                this.addLabel(`Reg ${i + 1}`, sphere.position);
            }
        }

        console.log(`✅ Creadas ${this.objects.length} esferas optimizadas`);
    }

    createNetworkGraph() {
        // Crear grafo de relaciones entre registros
        const center = new THREE.Vector3(0, 0, 0);
        const radius = 4;

        this.data.forEach((record, index) => {
            const angle = (index / this.data.length) * Math.PI * 2;
            const x = Math.cos(angle) * radius;
            const y = Math.sin(angle) * radius;
            const z = (Math.random() - 0.5) * 2;

            // Nodo
            const geometry = new THREE.OctahedronGeometry(0.15);
            const material = new THREE.MeshPhongMaterial({
                color: this.getColorForRecord(record, index)
            });

            const node = new THREE.Mesh(geometry, material);
            node.position.set(x, y, z);
            node.userData = { record, index };

            // Conexiones con nodos cercanos
            if (index > 0 && this.objects.length > 0) {
                const lastNode = this.objects[this.objects.length - 1];
                this.createConnection(lastNode.position, node.position);
            }

            this.scene.add(node);
            this.objects.push(node);

            // Etiqueta
            if (this.data.length <= 30) {
                this.addLabel(`${index + 1}`, node.position);
            }
        });
    }

    createConnection(startPos, endPos) {
        const direction = new THREE.Vector3().subVectors(endPos, startPos);
        const length = direction.length();
        const geometry = new THREE.CylinderGeometry(0.02, 0.02, length, 8);
        geometry.rotateZ(Math.PI / 2);

        const material = new THREE.MeshBasicMaterial({ color: 0x666666 });
        const connection = new THREE.Mesh(geometry, material);

        connection.position.copy(startPos);
        connection.position.add(direction.multiplyScalar(0.5));
        connection.lookAt(endPos);

        this.scene.add(connection);
        this.objects.push(connection);
    }

    createTimeline() {
        const timeScale = 8;
        const verticalSpread = 3;

        this.data.forEach((record, index) => {
            const x = (index / this.data.length - 0.5) * timeScale;
            const y = (Math.random() - 0.5) * verticalSpread;
            const z = (Math.random() - 0.5) * verticalSpread;

            const geometry = new THREE.BoxGeometry(0.1, 0.1, 0.1);
            const material = new THREE.MeshPhongMaterial({
                color: this.getColorForRecord(record, index)
            });

            const cube = new THREE.Mesh(geometry, material);
            cube.position.set(x, y, z);
            cube.userData = { record, index };

            this.scene.add(cube);
            this.objects.push(cube);
        });
    }

    createHierarchy() {
        const levels = 4;
        const itemsPerLevel = Math.ceil(this.data.length / levels);

        this.data.forEach((record, index) => {
            const level = Math.floor(index / itemsPerLevel);
            const positionInLevel = index % itemsPerLevel;

            const x = (positionInLevel / itemsPerLevel - 0.5) * 6;
            const y = (levels - level) * 1.5 - 3;
            const z = 0;

            const geometry = new THREE.ConeGeometry(0.1, 0.3, 8);
            const material = new THREE.MeshPhongMaterial({
                color: this.getColorForRecord(record, index)
            });

            const cone = new THREE.Mesh(geometry, material);
            cone.position.set(x, y, z);
            cone.rotation.x = Math.PI;
            cone.userData = { record, index };

            this.scene.add(cone);
            this.objects.push(cone);

            // Etiqueta
            if (this.data.length <= 40) {
                this.addLabel(`${index + 1}`, cone.position);
            }
        });
    }

    createCubeVisualization() {
        console.log('🎲 Creando visualización en cubo interactivo...');
        
        // Limpiar escena primero
        this.clearScene();
        this.clearLabels();
        
        const cubeSize = 8;
        const data = this.data;
        const totalPoints = data.length;
        
        // Crear cubo contenedor
        const cubeGeometry = new THREE.BoxGeometry(cubeSize, cubeSize, cubeSize);
        const cubeEdges = new THREE.EdgesGeometry(cubeGeometry);
        const cubeLines = new THREE.LineSegments(cubeEdges, 
            new THREE.LineBasicMaterial({ color: 0xffffff, transparent: true, opacity: 0.3 })
        );
        this.scene.add(cubeLines);
        this.objects.push(cubeLines);
        
        // Distribuir puntos en el cubo
        const gridSize = Math.ceil(Math.pow(totalPoints, 1/3)); // Raíz cúbica para distribución 3D
        
        data.forEach((record, index) => {
            // Calcular posición en grid 3D
            const x = (index % gridSize) - gridSize/2;
            const y = (Math.floor(index / gridSize) % gridSize) - gridSize/2;
            const z = (Math.floor(index / (gridSize * gridSize))) - gridSize/2;
            
            // Normalizar posiciones dentro del cubo
            const scale = cubeSize / gridSize;
            const posX = x * scale * 0.8;
            const posY = y * scale * 0.8;
            const posZ = z * scale * 0.8;
            
            // Crear esfera representando el dato
            const geometry = new THREE.SphereGeometry(0.1, 8, 8);
            const material = new THREE.MeshPhongMaterial({
                color: this.getColorForRecord(record, index),
                transparent: true,
                opacity: 0.8
            });
            
            const sphere = new THREE.Mesh(geometry, material);
            sphere.position.set(posX, posY, posZ);
            sphere.userData = { 
                record, 
                index,
                originalPosition: { x: posX, y: posY, z: posZ }
            };
            
            this.scene.add(sphere);
            this.objects.push(sphere);
        });
        
        // Agregar ejes de referencia dentro del cubo
        this.addCubeAxes(cubeSize);
        
        console.log(`✅ Cubo 3D creado con ${this.objects.length} objetos`);
    }

    addCubeAxes(size) {
        const axisLength = size * 0.8;
        
        // Eje X (Rojo)
        const xGeometry = new THREE.BufferGeometry().setFromPoints([
            new THREE.Vector3(-axisLength/2, 0, 0),
            new THREE.Vector3(axisLength/2, 0, 0)
        ]);
        const xMaterial = new THREE.LineBasicMaterial({ color: 0xff4444 });
        const xAxis = new THREE.Line(xGeometry, xMaterial);
        this.scene.add(xAxis);
        this.objects.push(xAxis);
        
        // Eje Y (Verde)
        const yGeometry = new THREE.BufferGeometry().setFromPoints([
            new THREE.Vector3(0, -axisLength/2, 0),
            new THREE.Vector3(0, axisLength/2, 0)
        ]);
        const yMaterial = new THREE.LineBasicMaterial({ color: 0x44ff44 });
        const yAxis = new THREE.Line(yGeometry, yMaterial);
        this.scene.add(yAxis);
        this.objects.push(yAxis);
        
        // Eje Z (Azul)
        const zGeometry = new THREE.BufferGeometry().setFromPoints([
            new THREE.Vector3(0, 0, -axisLength/2),
            new THREE.Vector3(0, 0, axisLength/2)
        ]);
        const zMaterial = new THREE.LineBasicMaterial({ color: 0x4444ff });
        const zAxis = new THREE.Line(zGeometry, zMaterial);
        this.scene.add(zAxis);
        this.objects.push(zAxis);
    }

    addLabel(text, position) {
        // Método simple para etiquetas - versión corregida
        const canvas = document.createElement('canvas');
        const context = canvas.getContext('2d');
        canvas.width = 128;
        canvas.height = 64;

        // Fondo
        context.fillStyle = 'rgba(0, 0, 0, 0.8)';
        context.fillRect(0, 0, canvas.width, canvas.height);

        // Texto
        context.fillStyle = 'white';
        context.font = 'bold 14px Arial';
        context.textAlign = 'center';
        context.textBaseline = 'middle';
        context.fillText(text, canvas.width / 2, canvas.height / 2);

        const texture = new THREE.CanvasTexture(canvas);
        const material = new THREE.SpriteMaterial({ map: texture });
        const sprite = new THREE.Sprite(material);

        sprite.position.copy(position);
        sprite.position.y += 0.3;
        sprite.scale.set(1, 0.5, 1);

        this.scene.add(sprite);
        this.objects.push(sprite);
    }

    // 🔥 MÉTODO updateLabels CORREGIDO
    updateLabels() {
        // Método simplificado - si necesitas etiquetas complejas, implementa aquí
        // Por ahora, este método existe para evitar el error
        if (this.labels.length === 0) return;
        
        // Aquí iría la lógica para actualizar posiciones de etiquetas
        // si estuvieras usando un sistema de etiquetas DOM overlay
    }

    getColorForRecord(record, index) {
        // Generar colores basados en diferentes criterios
        const colorSchemes = [
            0x4ade80, 0x60a5fa, 0xf87171, 0xfbbf24, 0xa78bfa,
            0x2dd4bf, 0xfb7185, 0x38bdf8, 0xf472b6, 0x84cc16
        ];

        // Si hay campo de confianza, usar para color
        if (record.confidence || record.Confianza) {
            const conf = parseInt(record.confidence || record.Confianza) || 50;
            if (conf >= 80) return 0x4ade80; // Verde
            if (conf >= 60) return 0x60a5fa; // Azul
            return 0xf87171; // Rojo
        }

        // Color por índice como fallback
        return colorSchemes[index % colorSchemes.length];
    }

    clearScene() {
        this.objects.forEach(obj => {
            if (obj instanceof THREE.Object3D) {
                this.scene.remove(obj);
                if (obj.geometry) obj.geometry.dispose();
                if (obj.material) {
                    if (Array.isArray(obj.material)) {
                        obj.material.forEach(material => material.dispose());
                    } else {
                        obj.material.dispose();
                    }
                }
            }
        });
        this.objects = [];
    }

    clearLabels() {
        // Limpiar etiquetas DOM si las hay
        this.labels.forEach(label => {
            if (label.parentNode) {
                label.parentNode.removeChild(label);
            }
        });
        this.labels = [];
    }

    showNoDataMessage() {
        const noDataElement = document.getElementById('noData3D');
        if (noDataElement) {
            noDataElement.style.display = 'flex';
        }
    }

    hideNoDataMessage() {
        const noDataElement = document.getElementById('noData3D');
        if (noDataElement) {
            noDataElement.style.display = 'none';
        }
    }

    showLoading() {
        const loadingElement = document.getElementById('loading3D');
        if (loadingElement) {
            loadingElement.style.display = 'block';
        }
    }

    hideLoading() {
        const loadingElement = document.getElementById('loading3D');
        if (loadingElement) {
            loadingElement.style.display = 'none';
        }
    }

    showErrorMessage(message) {
        console.error('❌ Error 3D:', message);
        // Puedes mostrar una notificación en la UI aquí
        if (typeof showNotification === 'function') {
            showNotification('error', 'Error 3D', message);
        }
    }

    updateStats() {
        const statRecords = document.getElementById('statRecords');
        const statObjects = document.getElementById('statObjects');
        const statView = document.getElementById('statView');

        if (statRecords) statRecords.textContent = this.data.length;
        if (statObjects) statObjects.textContent = this.objects.length;
        if (statView) {
            const visType = document.getElementById('visualizationType')?.value || '3D';
            statView.textContent = visType.toUpperCase();
        }
    }

    updateLegend() {
        const legendContent = document.getElementById('legendContent');
        if (!legendContent) return;

        const visType = document.getElementById('visualizationType')?.value || 'scatter';

        let legendHTML = '';

        switch (visType) {
            case 'scatter':
                legendHTML = `
                    <div class="legend-item">
                        <div class="color-dot" style="background: #4ade80"></div>
                        <span>Alta Confianza (>80%)</span>
                    </div>
                    <div class="legend-item">
                        <div class="color-dot" style="background: #60a5fa"></div>
                        <span>Confianza Media (60-80%)</span>
                    </div>
                    <div class="legend-item">
                        <div class="color-dot" style="background: #f87171"></div>
                        <span>Baja Confianza (<60%)</span>
                    </div>
                `;
                break;
            case 'network':
                legendHTML = `
                    <div class="legend-item">
                        <div class="shape-dot octahedron"></div>
                        <span>Nodo de Datos</span>
                    </div>
                    <div class="legend-item">
                        <div class="line-example"></div>
                        <span>Conexión</span>
                    </div>
                `;
                break;
            case 'cube':
                legendHTML = `
                    <div class="legend-item">
                        <div class="color-dot" style="background: #4ade80"></div>
                        <span>Punto de Datos</span>
                    </div>
                    <div class="legend-item">
                        <div class="cube-outline"></div>
                        <span>Espacio 3D</span>
                    </div>
                `;
                break;
            default:
                legendHTML = `<div class="legend-item">Visualización ${visType}</div>`;
        }

        legendContent.innerHTML = legendHTML;
    }

    // 🔥 MÉTODO animate CORREGIDO
    animate() {
        this.animationId = requestAnimationFrame(() => this.animate());

        try {
            // Rotación suave automática
            if (this.objects.length > 0) {
                this.scene.rotation.y += 0.002;
            }

            // Actualizar controles si existen
            if (this.controls) {
                this.controls.update();
            }

            // Actualizar etiquetas si es necesario
            this.updateLabels();

            // Renderizar escena
            this.renderer.render(this.scene, this.camera);

        } catch (error) {
            console.error('Error en animación 3D:', error);
            // Detener animación si hay error
            if (this.animationId) {
                cancelAnimationFrame(this.animationId);
            }
        }
    }

    onWindowResize() {
        if (!this.camera || !this.renderer || !this.container) return;

        this.camera.aspect = this.container.clientWidth / this.container.clientHeight;
        this.camera.updateProjectionMatrix();
        this.renderer.setSize(this.container.clientWidth, this.container.clientHeight);
    }

    destroy() {
        if (this.animationId) {
            cancelAnimationFrame(this.animationId);
        }
        this.clearScene();
        this.clearLabels();
        
        // Limpiar event listeners
        if (this.container) {
            const resizeObserver = new ResizeObserver(() => {});
            resizeObserver.disconnect();
        }
    }
}

// ==================== INTEGRACIÓN CON EL SISTEMA EXISTENTE ====================

let visualizer3D = null;

function initialize3DVisualizer() {
    if (!visualizer3D) {
        try {
            console.log('🚀 Inicializando visualizador 3D...');
            
            // Verificar que el contenedor existe
            const container = document.getElementById('3dContainer');
            if (!container) {
                console.error('❌ Contenedor 3D no encontrado');
                return null;
            }
            
            // Verificar que Three.js está cargado
            if (typeof THREE === 'undefined') {
                console.error('❌ Three.js no está cargado');
                return null;
            }
            
            visualizer3D = new Prolog3DVisualizer('3dContainer');
            console.log('✅ Visualizador 3D inicializado correctamente');
            
            // Ocultar mensaje de no datos
            const noDataElement = document.getElementById('noData3D');
            if (noDataElement) {
                noDataElement.style.display = 'none';
            }
            
        } catch (error) {
            console.error('❌ Error inicializando visualizador 3D:', error);
            if (typeof showNotification === 'function') {
                showNotification('error', 'Error 3D', 'No se pudo inicializar el visualizador 3D');
            }
        }
    }
    return visualizer3D;
}


// Función principal para visualizar datos
function visualizeDataIn3D(data, headers = []) {
    console.log('🎯 Visualizando datos en 3D:', data);

    if (!data || data.length === 0) {
        if (typeof showNotification === 'function') {
            showNotification('warning', 'Sin datos', 'No hay datos para visualizar en 3D');
        }
        return;
    }

    const visualizer = initialize3DVisualizer();

    if (visualizer) {
        visualizer.visualizeAllData(data, headers);
        if (typeof showNotification === 'function') {
            showNotification('success', 'Visualización 3D',
                `${data.length} registros visualizados en 3D`);
        }
    }
}

// Visualizar resultados de consultas Prolog
// Visualizar resultados de consultas Prolog
function visualizePrologResultsIn3D(results, query) {
    if (!results || results.length === 0) {
        if (typeof showNotification === 'function') {
            showNotification('info', 'Sin resultados', 'La consulta no devolvió resultados para visualizar');
        }
        return;
    }

    console.log('🔍 Visualizando resultados Prolog en 3D:', results);
    visualizeDataIn3D(results);
}

// Actualizar visualización cuando cambian los datos
function refresh3DVisualization() {
    if (appState && appState.currentData && appState.currentData.length > 0) {
        visualizeDataIn3D(appState.currentData);
    } else {
        if (typeof showNotification === 'function') {
            showNotification('info', 'Sin datos', 'Carga un archivo primero para visualizar en 3D');
        }
    }
}

function changeVisualizationType() {
    refresh3DVisualization();
}

function update3DLayout() {
    refresh3DVisualization();
}

function toggle3DView() {
    const container = document.getElementById('3dContainer');
    if (container) {
        container.classList.toggle('fullscreen');
        
        if (visualizer3D) {
            setTimeout(() => {
                visualizer3D.onWindowResize();
            }, 300);
        }
    }
}

function export3DScene() {
    if (!visualizer3D) {
        if (typeof showNotification === 'function') {
            showNotification('warning', 'Exportar', 'No hay visualización 3D activa');
        }
        return;
    }

    try {
        visualizer3D.renderer.domElement.toBlob(function (blob) {
            const url = URL.createObjectURL(blob);
            const a = document.createElement('a');
            a.href = url;
            a.download = `visualizacion-3d-${new Date().getTime()}.png`;
            document.body.appendChild(a);
            a.click();
            document.body.removeChild(a);
            URL.revokeObjectURL(url);
        });

        if (typeof showNotification === 'function') {
            showNotification('success', 'Exportado', 'Visualización 3D exportada como imagen');
        }
    } catch (error) {
        console.error('Error exportando escena 3D:', error);
        if (typeof showNotification === 'function') {
            showNotification('error', 'Error exportando', 'No se pudo exportar la visualización 3D');
        }
    }
}

// Inicializar automáticamente cuando hay datos
function setup3DAutoVisualization() {
    // Visualizar datos existentes si los hay
    if (appState && appState.currentData && appState.currentData.length > 0) {
        setTimeout(() => {
            visualizeDataIn3D(appState.currentData);
        }, 1000);
    }
}

// Inicializar cuando se carga la página
if (document.readyState === 'loading') {
    document.addEventListener('DOMContentLoaded', function() {
        console.log('🎯 Inicializando sistema 3D...');
        setTimeout(() => {
            initialize3DVisualizer();
            setup3DAutoVisualization();
        }, 2000);
    });
} else {
    // DOM ya está listo
    console.log('🎯 Inicializando sistema 3D (DOM listo)...');
    setTimeout(() => {
        initialize3DVisualizer();
        setup3DAutoVisualization();
    }, 1000);
}

// Función global para inicialización desde HTML
if (typeof window !== 'undefined') {
    window.init3DVisualization = function(data, headers) {
        console.log('🎨 Inicializando visualización 3D desde ventana global');
        visualizeDataIn3D(data, headers);
    };

    // Hacer funciones disponibles globalmente
    window.visualizeDataIn3D = visualizeDataIn3D;
    window.refresh3DVisualization = refresh3DVisualization;
    window.toggle3DView = toggle3DView;
    window.export3DScene = export3DScene;
    window.changeVisualizationType = changeVisualizationType;
    window.update3DLayout = update3DLayout;
}

// Inicializar cuando se carga la página
document.addEventListener('DOMContentLoaded', function () {
    setTimeout(() => {
        initialize3DVisualizer();
        setup3DAutoVisualization();
    }, 2000);
});

// En 3d-visualizer.js - modifica la inicialización
function initialize3DVisualizer() {
    if (!visualizer3D) {
        try {
            console.log('🚀 Inicializando visualizador 3D...');

            // Verificar que el contenedor existe
            const container = document.getElementById('3dContainer');
            if (!container) {
                console.error('❌ Contenedor 3D no encontrado');
                return null;
            }

            // Verificar que Three.js está cargado
            if (typeof THREE === 'undefined') {
                console.error('❌ Three.js no está cargado');
                return null;
            }

            visualizer3D = new Prolog3DVisualizer('3dContainer');
            console.log('✅ Visualizador 3D inicializado correctamente');

            // Ocultar mensaje de no datos
            const noDataElement = document.getElementById('noData3D');
            if (noDataElement) {
                noDataElement.style.display = 'none';
            }

        } catch (error) {
            console.error('❌ Error inicializando visualizador 3D:', error);
            showNotification('error', 'Error 3D', 'No se pudo inicializar el visualizador 3D');
        }
    }
    return visualizer3D;
}

// En la clase Prolog3DVisualizer, agregar el método createCubeVisualization
function createCubeVisualization() {
    console.log('🎲 Creando visualización en cubo interactivo...');

    // Limpiar escena primero
    this.clearScene();

    const cubeSize = 8;
    const data = this.data;
    const totalPoints = data.length;

    // Crear cubo contenedor
    const cubeGeometry = new THREE.BoxGeometry(cubeSize, cubeSize, cubeSize);
    const cubeEdges = new THREE.EdgesGeometry(cubeGeometry);
    const cubeLines = new THREE.LineSegments(cubeEdges,
        new THREE.LineBasicMaterial({ color: 0xffffff, transparent: true, opacity: 0.3 })
    );
    this.scene.add(cubeLines);
    this.objects.push(cubeLines);

    // Distribuir puntos en el cubo
    const gridSize = Math.ceil(Math.pow(totalPoints, 1 / 3)); // Raíz cúbica para distribución 3D

    data.forEach((record, index) => {
        // Calcular posición en grid 3D
        const x = (index % gridSize) - gridSize / 2;
        const y = (Math.floor(index / gridSize) % gridSize) - gridSize / 2;
        const z = (Math.floor(index / (gridSize * gridSize))) - gridSize / 2;

        // Normalizar posiciones dentro del cubo
        const scale = cubeSize / gridSize;
        const posX = x * scale * 0.8;
        const posY = y * scale * 0.8;
        const posZ = z * scale * 0.8;

        // Crear esfera representando el dato
        const geometry = new THREE.SphereGeometry(0.1, 8, 8);
        const material = new THREE.MeshPhongMaterial({
            color: this.getColorForRecord(record, index),
            transparent: true,
            opacity: 0.8
        });

        const sphere = new THREE.Mesh(geometry, material);
        sphere.position.set(posX, posY, posZ);
        sphere.userData = {
            record,
            index,
            originalPosition: { x: posX, y: posY, z: posZ }
        };

        // Agregar interactividad
        sphere.cursor = 'pointer';

        this.scene.add(sphere);
        this.objects.push(sphere);

        // Agregar etiqueta para puntos importantes
        if (totalPoints <= 50 || index % Math.ceil(totalPoints / 20) === 0) {
            this.addDataLabel(record, sphere.position, index);
        }
    });

    // Agregar ejes de referencia dentro del cubo
    this.addCubeAxes(cubeSize);

    console.log(`✅ Cubo 3D creado con ${this.objects.length} objetos`);
}

function addCubeAxes(size) {
    const axisLength = size * 0.8;

    // Eje X (Rojo)
    const xGeometry = new THREE.BufferGeometry().setFromPoints([
        new THREE.Vector3(-axisLength / 2, 0, 0),
        new THREE.Vector3(axisLength / 2, 0, 0)
    ]);
    const xMaterial = new THREE.LineBasicMaterial({ color: 0xff4444 });
    const xAxis = new THREE.Line(xGeometry, xMaterial);
    this.scene.add(xAxis);
    this.objects.push(xAxis);

    // Eje Y (Verde)
    const yGeometry = new THREE.BufferGeometry().setFromPoints([
        new THREE.Vector3(0, -axisLength / 2, 0),
        new THREE.Vector3(0, axisLength / 2, 0)
    ]);
    const yMaterial = new THREE.LineBasicMaterial({ color: 0x44ff44 });
    const yAxis = new THREE.Line(yGeometry, yMaterial);
    this.scene.add(yAxis);
    this.objects.push(yAxis);

    // Eje Z (Azul)
    const zGeometry = new THREE.BufferGeometry().setFromPoints([
        new THREE.Vector3(0, 0, -axisLength / 2),
        new THREE.Vector3(0, 0, axisLength / 2)
    ]);
    const zMaterial = new THREE.LineBasicMaterial({ color: 0x4444ff });
    const zAxis = new THREE.Line(zGeometry, zMaterial);
    this.scene.add(zAxis);
    this.objects.push(zAxis);
}

function addDataLabel(record, position, index) {
    const label = document.createElement('div');
    label.className = 'data-label-3d';
    label.innerHTML = `
        <div class="label-content">
            <strong>Registro ${index + 1}</strong>
            ${this.getRecordPreview(record)}
        </div>
    `;
    label.style.cssText = `
        position: absolute;
        color: white;
        background: rgba(0,0,0,0.8);
        padding: 5px;
        border-radius: 3px;
        font-size: 12px;
        pointer-events: none;
        transform: translate(-50%, -100%);
        white-space: nowrap;
        display: none;
    `;

    document.getElementById('3dContainer').appendChild(label);

    // Guardar referencia para actualizar posición
    const labelObj = { element: label, position };
    this.objects.push(labelObj);
}

function updateLabels() {
    this.objects.forEach(obj => {
        if (obj.element) {
            // Convertir posición 3D a coordenadas de pantalla
            const vector = obj.position.clone();
            vector.project(this.camera);
            
            const container = document.getElementById('3dContainer');
            const x = (vector.x * 0.5 + 0.5) * container.clientWidth;
            const y = (-(vector.y * 0.5 + 0.5)) * container.clientHeight;
            
            if (vector.z < 1) {
                obj.element.style.display = 'block';
                obj.element.style.left = x + 'px';
                obj.element.style.top = y + 'px';
            } else {
                obj.element.style.display = 'none';
            }
        }
    });
}

function getRecordPreview(record) {
    const previewItems = [];
    let count = 0;
    
    for (const [key, value] of Object.entries(record)) {
        if (count < 2 && value !== null && value !== undefined) {
            const strValue = String(value);
            if (strValue.length <= 20) {
                previewItems.push(`${key}: ${strValue}`);
                count++;
            }
        }
    }
    
    return previewItems.length > 0 ? 
        `<div style="font-size: 10px;">${previewItems.join('<br>')}</div>` : '';
}
