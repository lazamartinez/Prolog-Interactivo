class PrologVisualizer {
    constructor() {
        this.engine = new PrologEngine();
        this.renderer = new TreeRenderer('logicCanvas');
        this.currentProgram = '';
        this.isStepMode = false;
        this.animationSpeed = 5;

        this.init();
    }

    init() {
        this.bindEvents();
        this.loadExampleProgram();
    }

    bindEvents() {
        // Botones principales
        document.getElementById('loadExample').addEventListener('click', () => {
            this.loadExampleProgram();
        });

        document.getElementById('uploadFile').addEventListener('click', () => {
            this.showFileModal();
        });

        document.getElementById('executeQuery').addEventListener('click', () => {
            this.executeCurrentQuery();
        });

        document.getElementById('stepMode').addEventListener('click', () => {
            this.toggleStepMode();
        });

        // Controles del canvas
        document.getElementById('zoomIn').addEventListener('click', () => {
            this.renderer.camera.scale *= 1.2;
        });

        document.getElementById('zoomOut').addEventListener('click', () => {
            this.renderer.camera.scale /= 1.2;
        });

        document.getElementById('resetView').addEventListener('click', () => {
            this.renderer.resetView();
        });

        // Modal de archivo
        document.getElementById('fileInput').addEventListener('change', (e) => {
            this.handleFileUpload(e.target.files[0]);
        });

        document.getElementById('loadFromText').addEventListener('click', () => {
            this.loadFromText();
        });

        document.getElementById('loadFromFile').addEventListener('click', () => {
            document.getElementById('fileInput').click();
        });

        document.querySelector('.modal-close').addEventListener('click', () => {
            this.hideFileModal();
        });

        // Eventos del renderer
        this.renderer.canvas.addEventListener('nodeSelected', (e) => {
            this.onNodeClick(e.detail.node);
        });

        // Cerrar modal haciendo clic fuera
        document.getElementById('fileModal').addEventListener('click', (e) => {
            if (e.target.id === 'fileModal') {
                this.hideFileModal();
            }
        });
    }

    async loadExampleProgram() {
        this.showLoading();
        console.log('Cargando programa de ejemplo...');

        try {
            const exampleProgram = this.engine.generateExampleProgram();
            await this.engine.loadProgram(exampleProgram);
            this.currentProgram = exampleProgram;

            this.updateCodeDisplay(exampleProgram);
            this.generateVisualTree();
            this.showNotification('Programa de ejemplo cargado exitosamente', 'success');
            
            this.engine.printPredicateIndex();
        } catch (error) {
            console.error('Error cargando ejemplo:', error);
            this.showNotification(`Error: ${error.message}`, 'error');
        } finally {
            this.hideLoading();
        }
    }
    
    generateVisualTree() {
        try {
            console.log('=== INICIANDO GENERACIÓN DE ÁRBOL VISUAL ===');
            const treeData = this.engine.generateRuleTree();
            
            console.log('Datos del árbol generados:', {
                nodes: treeData.nodes.length,
                connections: treeData.connections.length,
                tiposDeNodos: {
                    rules: treeData.nodes.filter(n => n.type === 'rule').length,
                    facts: treeData.nodes.filter(n => n.type === 'fact').length,
                    goals: treeData.nodes.filter(n => n.type === 'predicate' || n.type === 'variable').length
                },
                tiposDeConexiones: {
                    contains: treeData.connections.filter(c => c.type === 'contains').length,
                    resolves_with_rule: treeData.connections.filter(c => c.type === 'resolves_with_rule').length,
                    resolves_with_fact: treeData.connections.filter(c => c.type === 'resolves_with_fact').length
                }
            });
            
            this.renderer.visualizePrologTree(treeData);
            
            this.showNotification(`Árbol generado: ${treeData.nodes.length} nodos, ${treeData.connections.length} conexiones`, 'success');
        } catch (error) {
            console.error('Error generando árbol visual:', error);
            this.showNotification('Error generando visualización del árbol', 'error');
        }
    }

    onNodeClick(node) {
        console.log('Nodo clickeado:', node);
        
        let info = `
        <div class="node-detail">
            <h4>${node.label}</h4>
            <p><strong>Tipo:</strong> ${node.type}</p>
            ${node.original ? `<p><strong>Código:</strong> ${node.original}</p>` : ''}
            ${node.predicate ? `<p><strong>Predicado:</strong> ${node.predicate}/${node.arity}</p>` : ''}
            <p><strong>ID:</strong> ${node.id}</p>
        </div>
        `;

        this.updateNodeInfo(info);
        this.highlightNodeConnections(node.id);
    }

    highlightNodeConnections(nodeId) {
        this.renderer.connections.forEach(conn => {
            if (conn.type === 'highlight') {
                conn.type = conn.meta?.originalType || 'normal';
            }
        });

        this.renderer.connections.forEach(conn => {
            if (conn.from === nodeId || conn.to === nodeId) {
                conn.meta = conn.meta || {};
                conn.meta.originalType = conn.type;
                conn.type = 'highlight';
            }
        });

        this.renderer.render();
    }

    showFileModal() {
        document.getElementById('fileModal').style.display = 'block';
        document.getElementById('prologInput').value = this.currentProgram;
    }

    hideFileModal() {
        document.getElementById('fileModal').style.display = 'none';
    }

    async handleFileUpload(file) {
        if (!file) return;

        this.showLoading();

        try {
            const text = await this.readFileAsText(file);
            await this.engine.loadProgram(text);
            this.currentProgram = text;

            this.updateCodeDisplay(text);
            this.generateVisualTree();
            this.hideFileModal();
            this.showNotification('Archivo cargado exitosamente', 'success');
        } catch (error) {
            this.showNotification(`Error cargando archivo: ${error.message}`, 'error');
        } finally {
            this.hideLoading();
        }
    }

    async loadFromText() {
        const text = document.getElementById('prologInput').value;

        if (!text.trim()) {
            this.showNotification('Por favor ingresa código Prolog', 'warning');
            return;
        }

        this.showLoading();

        try {
            await this.engine.loadProgram(text);
            this.currentProgram = text;

            this.updateCodeDisplay(text);
            this.generateVisualTree();
            this.hideFileModal();
            this.showNotification('Código cargado exitosamente', 'success');
        } catch (error) {
            this.showNotification(`Error: ${error.message}`, 'error');
        } finally {
            this.hideLoading();
        }
    }

    readFileAsText(file) {
        return new Promise((resolve, reject) => {
            const reader = new FileReader();
            reader.onload = (e) => resolve(e.target.result);
            reader.onerror = (e) => reject(new Error('Error leyendo archivo'));
            reader.readAsText(file);
        });
    }

    async executeCurrentQuery() {
        const query = document.getElementById('prologQuery').value.trim();

        if (!query) {
            this.showNotification('Por favor ingresa una consulta Prolog', 'warning');
            return;
        }

        this.showExecutionProgress();

        try {
            const results = await this.engine.executeQuery(query, (step) => {
                this.updateExecutionProgress(step);
            });

            this.hideExecutionProgress();

            if (results.length === 0) {
                this.showNotification('La consulta no produjo resultados', 'info');
            } else {
                this.showNotification(`Consulta ejecutada: ${results.length} resultado(s) encontrado(s)`, 'success');
                console.log('Resultados:', results);
            }
        } catch (error) {
            this.hideExecutionProgress();
            this.showNotification(`Error en consulta: ${error.message}`, 'error');
        }
    }

    toggleStepMode() {
        this.isStepMode = !this.isStepMode;
        const button = document.getElementById('stepMode');

        if (this.isStepMode) {
            button.classList.add('active');
            button.innerHTML = '<i class="fas fa-pause"></i> Modo Continuo';
            this.showNotification('Modo paso a paso activado', 'info');
        } else {
            button.classList.remove('active');
            button.innerHTML = '<i class="fas fa-footsteps"></i> Modo Paso a Paso';
            this.showNotification('Modo continuo activado', 'info');
        }
    }

    updateCodeDisplay(code) {
        document.getElementById('prologCode').innerHTML =
            `<pre><code>${this.escapeHtml(code)}</code></pre>`;
    }

    updateNodeInfo(info) {
        document.getElementById('nodeInfo').innerHTML = info;
    }

    showExecutionProgress() {
        document.getElementById('executionProgress').style.display = 'block';
    }

    updateExecutionProgress(step) {
        const progressBar = document.querySelector('.progress-fill');
        const progressText = document.querySelector('.progress-text');

        if (step.type === 'complete') {
            progressBar.style.width = '100%';
            progressText.textContent = 'Consulta completada';
        } else if (step.type === 'error') {
            progressBar.style.width = '100%';
            progressBar.style.background = '#ef4444';
            progressText.textContent = `Error: ${step.error}`;
        } else {
            const progress = (step.step / 20) * 100;
            progressBar.style.width = `${Math.min(progress, 90)}%`;
            progressText.textContent = `Ejecutando paso ${step.step}...`;
        }
    }

    hideExecutionProgress() {
        setTimeout(() => {
            document.getElementById('executionProgress').style.display = 'none';
            document.querySelector('.progress-fill').style.width = '0%';
            document.querySelector('.progress-fill').style.background = '#10b981';
        }, 1000);
    }

    showLoading() {
        document.getElementById('loadingOverlay').style.display = 'flex';
    }

    hideLoading() {
        document.getElementById('loadingOverlay').style.display = 'none';
    }

    showNotification(message, type = 'info') {
        const notification = document.createElement('div');
        notification.className = `notification ${type}`;
        notification.innerHTML = `
            <div class="notification-content">${message}</div>
        `;

        notification.style.cssText = `
            position: fixed;
            top: 100px;
            right: 20px;
            background: ${this.getNotificationColor(type)};
            color: white;
            padding: 1rem 1.5rem;
            border-radius: 8px;
            box-shadow: 0 4px 6px -1px rgba(0, 0, 0, 0.1);
            z-index: 1000;
            max-width: 400px;
            animation: slideIn 0.3s ease;
        `;

        document.body.appendChild(notification);

        setTimeout(() => {
            notification.style.animation = 'slideOut 0.3s ease';
            setTimeout(() => {
                if (document.body.contains(notification)) {
                    document.body.removeChild(notification);
                }
            }, 300);
        }, 3000);
    }

    getNotificationColor(type) {
        const colors = {
            success: '#10b981',
            error: '#ef4444',
            warning: '#f59e0b',
            info: '#3b82f6'
        };
        return colors[type] || '#3b82f6';
    }

    escapeHtml(text) {
        const div = document.createElement('div');
        div.textContent = text;
        return div.innerHTML;
    }
}

// Inicializar la aplicación cuando el DOM esté listo
document.addEventListener('DOMContentLoaded', () => {
    console.log('Inicializando PrologVisualizer...');
    window.visualizer = new PrologVisualizer();
});