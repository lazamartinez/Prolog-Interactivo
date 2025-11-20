class TreeRenderer {
    constructor(canvasId) {
        this.canvas = document.getElementById(canvasId);
        this.ctx = this.canvas.getContext('2d');
        this.nodes = new Map();
        this.connections = [];
        
        this.camera = {
            x: 0,
            y: 0,
            scale: 1
        };
        
        this.dragging = false;
        this.lastMouse = { x: 0, y: 0 };
        this.selectedNode = null;
        this.highlightedNodes = new Set();
        
        this.colors = {
            fact: '#10b981',
            rule: '#3b82f6', 
            predicate: '#f59e0b',
            variable: '#8b5cf6',
            failure: '#ef4444',
            success: '#10b981',
            connection: '#94a3b8',
            connectionActive: '#6366f1',
            connectionSuccess: '#10b981',
            connectionFailure: '#ef4444',
            connectionHighlight: '#f59e0b',
            text: '#f8fafc',
            background: '#0f172a',
            nodeHighlight: 'rgba(99, 102, 241, 0.3)',
            nodeSelected: 'rgba(139, 92, 246, 0.4)'
        };

        this.connectionStyles = {
            'contains': { color: '#94a3b8', width: 2, dash: [], arrow: false },
            'resolves_with_rule': { color: '#8b5cf6', width: 3, dash: [], arrow: true },
            'resolves_with_fact': { color: '#10b981', width: 3, dash: [5, 2], arrow: true },
            'normal': { color: '#94a3b8', width: 2, dash: [], arrow: false },
            'success': { color: '#10b981', width: 3, dash: [], arrow: true },
            'failure': { color: '#ef4444', width: 2, dash: [5, 3], arrow: false },
            'active': { color: '#6366f1', width: 4, dash: [], arrow: true },
            'highlight': { color: '#f59e0b', width: 3, dash: [], arrow: true }
        };
        
        this.init();
    }


    init() {
        this.resizeCanvas();
        window.addEventListener('resize', () => this.resizeCanvas());
        
        // Eventos del mouse
        this.canvas.addEventListener('mousedown', (e) => this.onMouseDown(e));
        this.canvas.addEventListener('mousemove', (e) => this.onMouseMove(e));
        this.canvas.addEventListener('mouseup', () => this.onMouseUp());
        this.canvas.addEventListener('wheel', (e) => this.onWheel(e));
        this.canvas.addEventListener('click', (e) => this.onCanvasClick(e));
        this.canvas.addEventListener('dblclick', (e) => this.onCanvasDoubleClick(e));
        
        this.animate();
    }

    getConnectionStyle(connection) {
        // Primero buscar en los estilos predefinidos
        const baseStyle = this.connectionStyles[connection.type] || this.connectionStyles.normal;
        
        let style = { ...baseStyle };
        
        // Aplicar efectos especiales basados en metadata
        if (connection.meta) {
            if (connection.meta.connectionType === 'goal_to_rule') {
                style.color = '#8b5cf6'; // Color morado para goals -> rules
                style.width = 3;
                style.arrow = true;
                style.glow = 'rgba(139, 92, 246, 0.4)';
            } else if (connection.meta.connectionType === 'goal_to_fact') {
                style.color = '#10b981'; // Color verde para goals -> facts
                style.width = 3;
                style.arrow = true;
                style.dash = [5, 2];
                style.glow = 'rgba(16, 185, 129, 0.4)';
            }
            
            if (connection.meta.matchType === 'same_predicate_different_arity') {
                style.dash = [3, 3];
                style.width = 2;
                style.color = '#f59e0b'; // Color amarillo para coincidencias parciales
            }
        }
        
        return style;
    }

    renderConnection(connection) {
        const fromNode = this.nodes.get(connection.from);
        const toNode = this.nodes.get(connection.to);
        
        if (!fromNode || !toNode) {
            console.warn(`Conexión inválida: ${connection.from} -> ${connection.to}`);
            return;
        }

        const start = this.getNodeEdgePoint(fromNode, toNode);
        const end = this.getNodeEdgePoint(toNode, fromNode);

        const style = this.getConnectionStyle(connection);
        
        this.ctx.strokeStyle = style.color;
        this.ctx.lineWidth = style.width * this.camera.scale;
        this.ctx.setLineDash(style.dash || []);
        this.ctx.lineCap = 'round';
        this.ctx.lineJoin = 'round';

        // Efecto de sombra para conexiones especiales
        if (style.glow) {
            this.ctx.shadowColor = style.glow;
            this.ctx.shadowBlur = 8;
        }

        // Dibujar línea curva
        this.drawCurvedLine(start, end, style.curvature || 0.3);
        this.ctx.stroke();

        // Resetear efectos de sombra
        this.ctx.shadowColor = 'transparent';
        this.ctx.shadowBlur = 0;

        // Dibujar flecha si está habilitada
        if (style.arrow) {
            this.drawArrow(end.x, end.y, Math.atan2(end.y - start.y, end.x - start.x), style);
        }

        this.ctx.setLineDash([]);
    }

    renderConnection(connection) {
        const fromNode = this.nodes.get(connection.from);
        const toNode = this.nodes.get(connection.to);
        
        if (!fromNode || !toNode) {
            console.warn(`Conexión inválida: ${connection.from} -> ${connection.to}`);
            return;
        }

        const start = this.getNodeEdgePoint(fromNode, toNode);
        const end = this.getNodeEdgePoint(toNode, fromNode);

        const style = this.getConnectionStyle(connection);
        
        this.ctx.strokeStyle = style.color;
        this.ctx.lineWidth = style.width * this.camera.scale;
        this.ctx.setLineDash(style.dash || []);
        this.ctx.lineCap = 'round';
        this.ctx.lineJoin = 'round';

        // Efecto de sombra para conexiones especiales
        if (style.glow) {
            this.ctx.shadowColor = style.glow;
            this.ctx.shadowBlur = 8;
        }

        // Dibujar línea curva
        this.drawCurvedLine(start, end, style.curvature || 0.3);
        this.ctx.stroke();

        // Resetear efectos de sombra
        this.ctx.shadowColor = 'transparent';
        this.ctx.shadowBlur = 0;

        // Dibujar flecha si está habilitada
        if (style.arrow) {
            this.drawArrow(end.x, end.y, Math.atan2(end.y - start.y, end.x - start.x), style);
        }

        this.ctx.setLineDash([]);
    }

    visualizePrologTree(treeData) {
        this.clear();
        
        console.log('Visualizando árbol Prolog:', {
            nodes: treeData.nodes.length,
            connections: treeData.connections.length
        });
        
        // Cargar nodos del árbol
        treeData.nodes.forEach(node => {
            this.createNode(
                node.id,
                node.type,
                node.label,
                node.x,
                node.y,
                { 
                    original: node.original,
                    prologData: node.prologData,
                    predicate: node.predicate,
                    arity: node.arity,
                    level: node.level
                }
            );
        });
        
        // Cargar conexiones del árbol
        treeData.connections.forEach(conn => {
            this.createConnection(conn.from, conn.to, conn.type, {
                id: conn.id,
                meta: conn.meta
            });
        });
        
        console.log('Nodos creados en renderer:', this.nodes.size);
        console.log('Conexiones creadas en renderer:', this.connections.length);
        
        // Mostrar información detallada de las conexiones
        this.connections.forEach(conn => {
            const fromNode = this.nodes.get(conn.from);
            const toNode = this.nodes.get(conn.to);
            console.log(`Conexión: ${fromNode?.label} -> ${toNode?.label} (${conn.type})`);
        });
        
        this.centerOnNodes();
        this.render();
        
        console.log('🌳 Árbol Prolog visualizado:', {
            nodes: this.nodes.size,
            connections: this.connections.length,
            types: {
                rules: Array.from(this.nodes.values()).filter(n => n.type === 'rule').length,
                facts: Array.from(this.nodes.values()).filter(n => n.type === 'fact').length,
                goals: Array.from(this.nodes.values()).filter(n => n.type === 'predicate' || n.type === 'variable').length
            }
        });
    }

    resizeCanvas() {
        const rect = this.canvas.getBoundingClientRect();
        this.canvas.width = rect.width;
        this.canvas.height = rect.height;
        this.render();
    }

    onMouseDown(e) {
        this.dragging = true;
        this.lastMouse = this.getMousePos(e);
        this.canvas.style.cursor = 'grabbing';
    }

    onMouseMove(e) {
        const mouse = this.getMousePos(e);
        
        // Cambiar cursor si está sobre un nodo
        const worldPos = this.screenToWorld(mouse.x, mouse.y);
        let overNode = false;
        this.nodes.forEach(node => {
            if (this.isPointInNode(worldPos.x, worldPos.y, node)) {
                overNode = true;
            }
        });
        this.canvas.style.cursor = overNode ? 'pointer' : (this.dragging ? 'grabbing' : 'grab');

        if (this.dragging) {
            const dx = mouse.x - this.lastMouse.x;
            const dy = mouse.y - this.lastMouse.y;
            
            this.camera.x += dx / this.camera.scale;
            this.camera.y += dy / this.camera.scale;
            
            this.lastMouse = mouse;
        }
    }

    onMouseUp() {
        this.dragging = false;
        this.canvas.style.cursor = 'grab';
    }

    onWheel(e) {
        e.preventDefault();
        const zoomIntensity = 0.1;
        const mouse = this.getMousePos(e);
        const wheel = e.deltaY < 0 ? 1 : -1;
        const zoom = Math.exp(wheel * zoomIntensity);
        
        const worldMouse = this.screenToWorld(mouse.x, mouse.y);
        this.camera.x -= (mouse.x / this.camera.scale - this.camera.x) * (1 - zoom);
        this.camera.y -= (mouse.y / this.camera.scale - this.camera.y) * (1 - zoom);
        this.camera.scale *= zoom;
        
        this.camera.scale = Math.max(0.05, Math.min(8, this.camera.scale));
    }

    onCanvasClick(e) {
        const mouse = this.getMousePos(e);
        const worldPos = this.screenToWorld(mouse.x, mouse.y);
        
        let clickedNode = null;
        this.nodes.forEach(node => {
            if (this.isPointInNode(worldPos.x, worldPos.y, node)) {
                clickedNode = node;
            }
        });
        
        if (clickedNode) {
            this.selectNode(clickedNode);
        } else {
            this.selectedNode = null;
            this.dispatchEvent('nodeDeselected');
        }
    }

    onCanvasDoubleClick(e) {
        const mouse = this.getMousePos(e);
        const worldPos = this.screenToWorld(mouse.x, mouse.y);
        
        this.nodes.forEach(node => {
            if (this.isPointInNode(worldPos.x, worldPos.y, node)) {
                this.dispatchEvent('nodeDoubleClick', { node });
            }
        });
    }

    selectNode(node) {
        this.selectedNode = node;
        this.dispatchEvent('nodeSelected', { node });
        this.render();
    }

    highlightNodes(nodeIds) {
        this.highlightedNodes = new Set(nodeIds);
        this.render();
    }

    clearHighlights() {
        this.highlightedNodes.clear();
        this.render();
    }

    isPointInNode(x, y, node) {
        return x >= node.x - node.width/2 && 
               x <= node.x + node.width/2 && 
               y >= node.y - node.height/2 && 
               y <= node.y + node.height/2;
    }

    dispatchEvent(eventName, detail = {}) {
        const event = new CustomEvent(eventName, { detail });
        this.canvas.dispatchEvent(event);
    }

    getMousePos(e) {
        const rect = this.canvas.getBoundingClientRect();
        return {
            x: e.clientX - rect.left,
            y: e.clientY - rect.top
        };
    }

    worldToScreen(x, y) {
        return {
            x: (x - this.camera.x) * this.camera.scale,
            y: (y - this.camera.y) * this.camera.scale
        };
    }

    screenToWorld(x, y) {
        return {
            x: x / this.camera.scale + this.camera.x,
            y: y / this.camera.scale + this.camera.y
        };
    }

    createNode(id, type, label, x, y, data = {}) {
        const node = {
            id,
            type,
            label,
            x,
            y,
            width: 140,
            height: 70,
            radius: 10,
            ...data,
            original: data.original || label
        };
        
        this.nodes.set(id, node);
        return node;
    }

    createConnection(fromId, toId, type = 'normal', data = {}) {
        // Verificar que los nodos existen
        if (!this.nodes.has(fromId) || !this.nodes.has(toId)) {
            console.warn(`No se puede crear conexión: nodos ${fromId} -> ${toId} no existen`);
            return null;
        }

        const connection = {
            id: `conn_${fromId}_${toId}_${Date.now()}`,
            from: fromId,
            to: toId,
            type,
            ...data
        };
        
        this.connections.push(connection);
        return connection;
    }

    render() {
        // Limpiar canvas con gradiente sutil
        const gradient = this.ctx.createLinearGradient(0, 0, this.canvas.width, this.canvas.height);
        gradient.addColorStop(0, '#0f172a');
        gradient.addColorStop(1, '#1e293b');
        this.ctx.fillStyle = gradient;
        this.ctx.fillRect(0, 0, this.canvas.width, this.canvas.height);

        // Renderizar conexiones PRIMERO (detrás de los nodos)
        this.connections.forEach(connection => {
            this.renderConnection(connection);
        });

        // Renderizar nodos DESPUÉS (encima de las conexiones)
        this.nodes.forEach(node => {
            this.renderNode(node);
        });
    }

    renderConnection(connection) {
        const fromNode = this.nodes.get(connection.from);
        const toNode = this.nodes.get(connection.to);
        
        if (!fromNode || !toNode) {
            return;
        }

        const start = this.getNodeEdgePoint(fromNode, toNode);
        const end = this.getNodeEdgePoint(toNode, fromNode);

        const style = this.getConnectionStyle(connection.type);
        
        this.ctx.strokeStyle = style.color;
        this.ctx.lineWidth = style.width * this.camera.scale;
        this.ctx.setLineDash(style.dash);
        this.ctx.lineCap = 'round';
        this.ctx.lineJoin = 'round';

        // Efecto de sombra para conexiones activas/resaltadas
        if (style.glow) {
            this.ctx.shadowColor = style.glow;
            this.ctx.shadowBlur = 8;
        }

        // Dibujar línea curva
        this.drawCurvedLine(start, end, style.curvature);
        this.ctx.stroke();

        // Resetear efectos de sombra
        this.ctx.shadowColor = 'transparent';
        this.ctx.shadowBlur = 0;

        // Dibujar flecha
        if (style.arrow) {
            this.drawArrow(end.x, end.y, Math.atan2(end.y - start.y, end.x - start.x), style);
        }

        this.ctx.setLineDash([]);
    }

    getNodeEdgePoint(node, targetNode) {
        const nodePos = this.worldToScreen(node.x, node.y);
        const targetPos = this.worldToScreen(targetNode.x, targetNode.y);
        
        const angle = Math.atan2(targetPos.y - nodePos.y, targetPos.x - nodePos.x);
        
        const halfWidth = (node.width * this.camera.scale) / 2;
        const halfHeight = (node.height * this.camera.scale) / 2;
        
        // Calcular la intersección con el borde del rectángulo
        const cos = Math.cos(angle);
        const sin = Math.sin(angle);
        
        // Determinar qué borde intersecta primero
        const tx = (cos > 0 ? halfWidth : -halfWidth) / cos;
        const ty = (sin > 0 ? halfHeight : -halfHeight) / sin;
        
        const t = Math.min(Math.abs(tx), Math.abs(ty));
        
        return {
            x: nodePos.x + cos * t,
            y: nodePos.y + sin * t
        };
    }

    drawCurvedLine(start, end, curvature = 0.3) {
        const midX = (start.x + end.x) / 2;
        const midY = (start.y + end.y) / 2;
        
        // Punto de control para la curva
        const dx = end.x - start.x;
        const dy = end.y - start.y;
        const controlX = midX - dy * curvature;
        const controlY = midY + dx * curvature;

        this.ctx.beginPath();
        this.ctx.moveTo(start.x, start.y);
        this.ctx.quadraticCurveTo(controlX, controlY, end.x, end.y);
    }

    drawArrow(x, y, angle, style) {
        const arrowLength = 12 * this.camera.scale;
        const arrowWidth = 8 * this.camera.scale;

        this.ctx.save();
        this.ctx.translate(x, y);
        this.ctx.rotate(angle);

        this.ctx.fillStyle = style.color;
        this.ctx.beginPath();
        this.ctx.moveTo(-arrowLength, -arrowWidth);
        this.ctx.lineTo(0, 0);
        this.ctx.lineTo(-arrowLength, arrowWidth);
        this.ctx.closePath();
        this.ctx.fill();

        this.ctx.restore();
    }

    getConnectionStyle(type) {
        const styles = {
            normal: {
                color: this.colors.connection,
                width: 2,
                dash: [],
                arrow: true,
                curvature: 0.2,
                glow: null
            },
            success: {
                color: this.colors.connectionSuccess,
                width: 3,
                dash: [],
                arrow: true,
                curvature: 0.3,
                glow: 'rgba(16, 185, 129, 0.4)'
            },
            failure: {
                color: this.colors.connectionFailure,
                width: 2,
                dash: [5, 3],
                arrow: false,
                curvature: 0.2,
                glow: null
            },
            active: {
                color: this.colors.connectionActive,
                width: 4,
                dash: [],
                arrow: true,
                curvature: 0.4,
                glow: 'rgba(99, 102, 241, 0.6)'
            },
            highlight: {
                color: this.colors.connectionHighlight,
                width: 3,
                dash: [],
                arrow: true,
                curvature: 0.3,
                glow: 'rgba(245, 158, 11, 0.4)'
            }
        };
        
        return styles[type] || styles.normal;
    }

    renderNode(node) {
        const screenPos = this.worldToScreen(node.x, node.y);
        const radius = Math.max(25, 35 * this.camera.scale); // Radio base para nodos redondos

        // Efecto de selección/resaltado (círculo exterior)
        if (this.selectedNode === node) {
            this.ctx.fillStyle = this.colors.nodeSelected;
            this.ctx.beginPath();
            this.ctx.arc(screenPos.x, screenPos.y, radius + 8, 0, Math.PI * 2);
            this.ctx.fill();
        }

        if (this.highlightedNodes.has(node.id)) {
            this.ctx.fillStyle = this.colors.nodeHighlight;
            this.ctx.beginPath();
            this.ctx.arc(screenPos.x, screenPos.y, radius + 5, 0, Math.PI * 2);
            this.ctx.fill();
        }

        // Sombra del nodo
        this.ctx.shadowColor = 'rgba(0, 0, 0, 0.3)';
        this.ctx.shadowBlur = 15 * this.camera.scale;
        this.ctx.shadowOffsetX = 0;
        this.ctx.shadowOffsetY = 3 * this.camera.scale;

        // Fondo del nodo con gradiente (círculo)
        const gradient = this.ctx.createRadialGradient(
            screenPos.x - radius/2, screenPos.y - radius/2, 0,
            screenPos.x, screenPos.y, radius
        );
        
        const baseColor = this.getNodeColor(node.type);
        const lightColor = this.lightenColor(baseColor, 30);
        const darkColor = this.darkenColor(baseColor, 10);
        
        gradient.addColorStop(0, lightColor);
        gradient.addColorStop(1, darkColor);

        this.ctx.fillStyle = gradient;
        this.ctx.beginPath();
        this.ctx.arc(screenPos.x, screenPos.y, radius, 0, Math.PI * 2);
        this.ctx.fill();

        // Borde del nodo
        this.ctx.strokeStyle = this.selectedNode === node ? '#ffffff' : this.darkenColor(baseColor, 20);
        this.ctx.lineWidth = (this.selectedNode === node ? 3 : 2) * this.camera.scale;
        this.ctx.stroke();

        // Resetear sombra
        this.ctx.shadowColor = 'transparent';
        this.ctx.shadowBlur = 0;

        // Texto del nodo
        this.renderNodeText(node, screenPos, radius);
    }

    getNodeColor(type) {
        return this.colors[type] || this.colors.predicate;
    }

    renderNodeText(node, screenPos, radius) {
        this.ctx.fillStyle = this.colors.text;
        this.ctx.font = `${Math.max(10, 12 * this.camera.scale)}px 'Segoe UI', Arial, sans-serif`;
        this.ctx.textAlign = 'center';
        this.ctx.textBaseline = 'middle';
        
        // Dividir el texto en líneas
        const maxWidth = radius * 1.6; // Ancho máximo basado en el radio
        const lines = this.wrapText(node.label, maxWidth);
        const lineHeight = Math.max(14, 16 * this.camera.scale);
        const totalHeight = lines.length * lineHeight;
        const startY = screenPos.y - totalHeight / 2 + lineHeight / 2;

        // Renderizar cada línea
        lines.forEach((line, index) => {
            this.ctx.fillText(line, screenPos.x, startY + index * lineHeight);
        });

        // Badge de tipo (más pequeño para nodos redondos)
        this.renderTypeBadge(node, screenPos, radius);
    }

    renderTypeBadge(node, screenPos, radius) {
        const typeText = node.type.toUpperCase();
        const badgeWidth = typeText.length * 6 * this.camera.scale + 8;
        const badgeHeight = 14 * this.camera.scale;
        
        this.ctx.fillStyle = this.darkenColor(this.getNodeColor(node.type), 30);
        this.ctx.beginPath();
        this.roundRect(this.ctx, 
            screenPos.x - badgeWidth/2, 
            screenPos.y + radius - badgeHeight/2, 
            badgeWidth, badgeHeight, 
            badgeHeight/2
        );
        this.ctx.fill();
        
        this.ctx.fillStyle = this.colors.text;
        this.ctx.font = `${Math.max(7, 9 * this.camera.scale)}px 'Segoe UI', Arial, sans-serif`;
        this.ctx.fillText(typeText, screenPos.x, screenPos.y + radius);
    }

    wrapText(text, maxWidth) {
        const words = text.split(' ');
        const lines = [];
        let currentLine = '';
        
        for (let word of words) {
            const testLine = currentLine ? currentLine + ' ' + word : word;
            const metrics = this.ctx.measureText(testLine);
            
            if (metrics.width > maxWidth && currentLine !== '') {
                lines.push(currentLine);
                currentLine = word;
            } else {
                currentLine = testLine;
            }
        }
        
        if (currentLine) {
            lines.push(currentLine);
        }
        
        // Si no cabe en una línea, truncar
        if (lines.length > 2) {
            lines.splice(2);
            if (lines[1]) {
                lines[1] = lines[1].substring(0, Math.min(lines[1].length, 15)) + '...';
            }
        }
        
        return lines;
    }

    renderTypeBadge(node, screenPos, width, height) {
        const typeText = node.type.toUpperCase();
        const badgeWidth = typeText.length * 7 * this.camera.scale + 10;
        const badgeHeight = 16 * this.camera.scale;
        
        this.ctx.fillStyle = this.darkenColor(this.getNodeColor(node.type), 30);
        this.ctx.beginPath();
        this.roundRect(this.ctx, 
            screenPos.x - badgeWidth/2, 
            screenPos.y + height/2 - badgeHeight - 5, 
            badgeWidth, badgeHeight, 
            badgeHeight/2
        );
        this.ctx.fill();
        
        this.ctx.fillStyle = this.colors.text;
        this.ctx.font = `${Math.max(8, 10 * this.camera.scale)}px 'Segoe UI', Arial, sans-serif`;
        this.ctx.fillText(typeText, screenPos.x, screenPos.y + height/2 - badgeHeight/2 - 5);
    }

    roundRect(ctx, x, y, width, height, radius) {
        ctx.beginPath();
        ctx.moveTo(x + radius, y);
        ctx.lineTo(x + width - radius, y);
        ctx.quadraticCurveTo(x + width, y, x + width, y + radius);
        ctx.lineTo(x + width, y + height - radius);
        ctx.quadraticCurveTo(x + width, y + height, x + width - radius, y + height);
        ctx.lineTo(x + radius, y + height);
        ctx.quadraticCurveTo(x, y + height, x, y + height - radius);
        ctx.lineTo(x, y + radius);
        ctx.quadraticCurveTo(x, y, x + radius, y);
        ctx.closePath();
    }

    lightenColor(color, percent) {
        const num = parseInt(color.replace("#", ""), 16);
        const amt = Math.round(2.55 * percent);
        const R = Math.min(255, (num >> 16) + amt);
        const G = Math.min(255, (num >> 8 & 0x00FF) + amt);
        const B = Math.min(255, (num & 0x0000FF) + amt);
        return "#" + ((1 << 24) + (R << 16) + (G << 8) + B).toString(16).slice(1);
    }

    darkenColor(color, percent) {
        const num = parseInt(color.replace("#", ""), 16);
        const amt = Math.round(2.55 * percent);
        const R = Math.max(0, (num >> 16) - amt);
        const G = Math.max(0, (num >> 8 & 0x00FF) - amt);
        const B = Math.max(0, (num & 0x0000FF) - amt);
        return "#" + ((1 << 24) + (R << 16) + (G << 8) + B).toString(16).slice(1);
    }

    animate() {
        this.render();
        requestAnimationFrame(() => this.animate());
    }

    resetView() {
        this.camera = { x: 0, y: 0, scale: 1 };
    }

    centerOnNodes(padding = 100) {
        if (this.nodes.size === 0) return;

        let minX = Infinity, maxX = -Infinity, minY = Infinity, maxY = -Infinity;
        
        this.nodes.forEach(node => {
            minX = Math.min(minX, node.x - node.width/2);
            maxX = Math.max(maxX, node.x + node.width/2);
            minY = Math.min(minY, node.y - node.height/2);
            maxY = Math.max(maxY, node.y + node.height/2);
        });

        const centerX = (minX + maxX) / 2;
        const centerY = (minY + maxY) / 2;
        const width = maxX - minX + padding * 2;
        const height = maxY - minY + padding * 2;

        const scaleX = this.canvas.width / width;
        const scaleY = this.canvas.height / height;
        this.camera.scale = Math.min(scaleX, scaleY, 1.5);
        
        this.camera.x = centerX;
        this.camera.y = centerY;
    }

    clear() {
        this.nodes.clear();
        this.connections = [];
        this.selectedNode = null;
        this.highlightedNodes.clear();
    }

    // ========== MÉTODOS DE VISUALIZACIÓN PROLOG ==========

    visualizePrologProgram(programStructure) {
        this.clear();
        
        const { facts, rules, predicates } = programStructure;
        let yOffset = -200;
        const xSpacing = 300;
        const ySpacing = 150;

        // Crear nodos para reglas
        rules.forEach((rule, index) => {
            const ruleId = `rule_${index}`;
            const ruleLabel = this.formatRuleLabel(rule);
            
            const ruleNode = this.createNode(
                ruleId,
                'rule',
                ruleLabel,
                0,
                yOffset + index * ySpacing,
                { 
                    original: rule.original,
                    prologData: rule
                }
            );

            // Crear nodos para el cuerpo de la regla
            rule.body.forEach((goal, goalIndex) => {
                const goalId = `goal_${index}_${goalIndex}`;
                const goalLabel = this.formatGoalLabel(goal);
                
                const goalNode = this.createNode(
                    goalId,
                    'predicate',
                    goalLabel,
                    xSpacing,
                    yOffset + index * ySpacing + (goalIndex - (rule.body.length - 1)/2) * 100,
                    { 
                        original: `${goal.predicate}(${goal.arguments.map(arg => arg.value).join(', ')})`,
                        prologData: goal
                    }
                );

                this.createConnection(ruleId, goalId, 'normal');
            });
        });

        // Crear nodos para hechos
        const factsStartY = yOffset + rules.length * ySpacing + 100;
        facts.forEach((fact, index) => {
            const factId = `fact_${index}`;
            const factLabel = this.formatFactLabel(fact);
            
            this.createNode(
                factId,
                'fact',
                factLabel,
                (index - (facts.length - 1)/2) * 200,
                factsStartY,
                { 
                    original: fact.original,
                    prologData: fact
                }
            );
        });

        this.centerOnNodes();
        this.render();
    }

    formatRuleLabel(rule) {
        const head = rule.head;
        const args = head.arguments.map(arg => 
            arg.type === 'variable' ? arg.value : `'${arg.value}'`
        ).join(', ');
        return `${head.predicate}(${args})`;
    }

    formatGoalLabel(goal) {
        const args = goal.arguments.map(arg => 
            arg.type === 'variable' ? arg.value : `'${arg.value}'`
        ).join(', ');
        return `${goal.predicate}(${args})`;
    }

    formatFactLabel(fact) {
        const args = fact.arguments.map(arg => 
            arg.type === 'variable' ? arg.value : `'${arg.value}'`
        ).join(', ');
        return `${fact.predicate}(${args})`;
    }

    // Método para visualizar ejecución de consultas
    visualizeQueryExecution(steps) {
        // Resaltar nodos involucrados en cada paso
        steps.forEach((step, index) => {
            setTimeout(() => {
                this.highlightStep(step);
            }, index * 1000);
        });
    }

    highlightStep(step) {
        // Limpiar highlights anteriores
        this.clearHighlights();
        
        // Resaltar nodos relevantes para este paso
        if (step.nodeIds && step.nodeIds.length > 0) {
            this.highlightNodes(step.nodeIds);
        }
        
        // Resaltar conexiones relevantes
        this.connections.forEach(conn => {
            if (step.connectionIds && step.connectionIds.includes(conn.id)) {
                conn.type = 'active';
            }
        });
        
        this.render();
        
        // Restaurar después de un tiempo
        setTimeout(() => {
            this.connections.forEach(conn => {
                if (conn.type === 'active') {
                    conn.type = 'normal';
                }
            });
            this.clearHighlights();
        }, 800);
    }

    // Método DEMO mejorado
    createDemoTree() {
        this.clear();

        // Nodo raíz principal
        const main = this.createNode('main', 'rule', 'clasificar_hongo(X, Clase)', 0, 0, {
            original: 'clasificar_hongo(X, Clase) :- es_comestible(X), tiene_sombrete(X).'
        });

        // Rama de éxito
        const edible = this.createNode('edible', 'rule', 'es_comestible(X)', -250, -120, {
            original: 'es_comestible(X) :- caracteristica(X, comestible).'
        });
        
        const safePred = this.createNode('safe_pred', 'predicate', 'tiene_sombrete(X)', -250, 120, {
            original: 'tiene_sombrete(X) :- caracteristica(X, sombrero_rojo).'
        });

        // Hechos
        const fact1 = this.createNode('fact1', 'fact', "caracteristica(boletus, 'comestible')", -450, -240, {
            original: "caracteristica(boletus, comestible)."
        });
        
        const fact2 = this.createNode('fact2', 'fact', "caracteristica(amanita, 'venenoso')", 450, -240, {
            original: "caracteristica(amanita, venenoso)."
        });

        // Rama de fallo
        const poison = this.createNode('poison', 'rule', 'es_venenoso(X)', 250, -120, {
            original: 'es_venenoso(X) :- caracteristica(X, venenoso).'
        });

        // Conexiones
        this.createConnection('main', 'edible', 'success');
        this.createConnection('main', 'safe_pred', 'success');
        this.createConnection('edible', 'fact1', 'normal');
        this.createConnection('poison', 'fact2', 'failure');

        this.centerOnNodes();
        this.render();
        
        console.log('🌳 Árbol de demostración creado con', this.nodes.size, 'nodos y', this.connections.length, 'conexiones');
    }
}