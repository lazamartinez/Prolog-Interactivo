// Estado global de la aplicación
const appState = {
  currentData: [],
  prologFacts: '',
  currentFile: null,
  sessionId: 'session_' + Date.now(),
  autonomousMode: true,
  threeDVisualizer: null
};

// Elementos DOM
const uploadArea = document.getElementById('uploadArea');
const fileInput = document.getElementById('fileInput');
const fileInfo = document.getElementById('fileInfo');
const alertDiv = document.getElementById('alert');
const loadingDiv = document.getElementById('loading');
const dataTable = document.getElementById('dataTable');
const prologQuery = document.getElementById('prologQuery');
const prologResults = document.getElementById('prologResults');
const prologOutput = document.getElementById('prologOutput');
const statsCard = document.getElementById('statsCard');
const statsGrid = document.getElementById('statsGrid');
const customRules = document.getElementById('customRules');

// Estado del carrusel
const carouselState = {
  currentIndex: 0,
  rulesPerView: 3,
  savedRules: [],
  currentRuleSet: []
};

// Estado del loading
const loadingState = {
  isShowing: false,
  currentType: 'default',
  progress: 0
};

// Agregar botón de diagnóstico al header
function addDiagnosticButtonToHeader() {
  const header = document.querySelector('header .header-content');
  if (header) {
    const diagnosticBtn = document.createElement('button');
    diagnosticBtn.className = 'btn btn-outline btn-sm';
    diagnosticBtn.innerHTML = '<i class="fas fa-stethoscope"></i> Diagnóstico del Sistema';
    diagnosticBtn.onclick = comprehensiveDiagnostic;
    diagnosticBtn.style.marginLeft = 'auto';
    diagnosticBtn.style.marginRight = '15px';

    header.appendChild(diagnosticBtn);
  }
}

// En tu DOMContentLoaded, agrega:
document.addEventListener('DOMContentLoaded', function() {
    initializeEventListeners();
    loadExamplePrologQueries();
    updateSessionInfo();
    setupQueryExamples(); // 🔥 NUEVO: Configurar ejemplos
    loadSavedQueries();
    loadSavedRulesList();
    loadRuleCarousel();
    setTimeout(check3DElements, 1000);
    
    // Sistema de rescate
    addUrgentDiagnosticButtons();
    addServerControlPanel();
    createNotificationContainer();
    updateServerStatusIndicator();
    
    // Probar servidor al inicio
    setTimeout(() => {
        testServerConnection();
        diagnoseServerState();
    }, 2000);
    
    console.log('🚀 Sistema completo inicializado');
});

// 🔥 NUEVA FUNCIÓN: Cargar carrusel de reglas
function loadRuleCarousel() {
  const savedRules = JSON.parse(localStorage.getItem('ruleCards') || '[]');
  carouselState.savedRules = savedRules;
  carouselState.currentRuleSet = savedRules;

  updateCarousel();
  updateCarouselInfo();
}

// 🔥 ACTUALIZAR: Función updateCarousel para usar la nueva createRuleCard
function updateCarousel() {
  const carousel = document.getElementById('rulesCarousel');
  const indicators = document.getElementById('carouselIndicators');

  if (!carousel || !indicators) return;

  // Limpiar carrusel
  carousel.innerHTML = '';
  indicators.innerHTML = '';

  if (carouselState.currentRuleSet.length === 0) {
    carousel.innerHTML = `
            <div class="empty-rules">
                <i class="fas fa-cards"></i>
                <p>No hay reglas individuales guardadas</p>
                <p class="text-muted">Guarda algunas reglas en el editor para verlas como cards individuales</p>
                <button class="btn btn-primary" onclick="generateRules()">
                    <i class="fas fa-magic"></i> Generar Reglas Automáticas
                </button>
            </div>
        `;
    return;
  }

  // Calcular índices para el viewport actual
  const startIndex = carouselState.currentIndex;
  const endIndex = Math.min(
    startIndex + carouselState.rulesPerView,
    carouselState.currentRuleSet.length
  );

  // Generar cards INDIVIDUALES
  for (let i = startIndex; i < endIndex; i++) {
    const rule = carouselState.currentRuleSet[i];
    const card = createRuleCard(rule, i);
    carousel.appendChild(card);
  }

  // Generar indicadores
  const totalPages = Math.ceil(carouselState.currentRuleSet.length / carouselState.rulesPerView);
  for (let i = 0; i < totalPages; i++) {
    const indicator = document.createElement('div');
    indicator.className = `carousel-indicator ${i === Math.floor(carouselState.currentIndex / carouselState.rulesPerView) ? 'active' : ''}`;
    indicator.onclick = () => goToPage(i);
    indicators.appendChild(indicator);
  }
}

// 🔥 ACTUALIZAR: Función generateRules para crear cards individuales automáticamente
async function generateRules() {
  try {
    const response = await fetch('/rules/generate', {
      method: 'POST',
      headers: {
        'Content-Type': 'application/json'
      },
      body: JSON.stringify({
        sessionId: appState.sessionId,
        criteria: ['classification', 'safety', 'context']
      })
    });

    const result = await response.json();

    if (result.success) {
      // 🔥 DIVIDIR REGLAS GENERADAS EN CARDS INDIVIDUALES
      const individualRules = splitRulesIntoIndividualCards(result.rules);

      if (individualRules.length === 0) {
        showNotification('warning', 'Sin reglas', 'No se generaron reglas individuales');
        return;
      }

      // Agregar cada regla individual al carrusel
      individualRules.forEach(rule => {
        rule.type = 'automática'; // Marcar como generada automáticamente
        carouselState.savedRules.unshift(rule);
      });

      carouselState.currentRuleSet = carouselState.savedRules;
      localStorage.setItem('ruleCards', JSON.stringify(carouselState.savedRules));

      updateCarousel();
      updateCarouselInfo();

      showNotification('success', 'Reglas Generadas',
        `Se crearon ${individualRules.length} cards individuales automáticamente`);

    } else {
      throw new Error(result.error);
    }
  } catch (error) {
    showNotification('error', 'Error',
      `No se pudieron generar las reglas: ${error.message}`);
  }
}

// 🔥 NUEVA FUNCIÓN: Crear card de regla individual MEJORADA
function createRuleCard(rule, index) {
  const card = document.createElement('div');
  card.className = 'rule-card';
  card.innerHTML = `
        <div class="rule-card-header">
            <h4 class="rule-card-title" title="${rule.name}">${rule.name}</h4>
            <div class="rule-card-actions">
                <button class="btn btn-outline btn-sm" onclick="copyRuleToClipboard('${rule.code.replace(/'/g, "\\'")}')" title="Copiar regla">
                    <i class="fas fa-copy"></i>
                </button>
                <button class="btn btn-outline btn-sm" onclick="editIndividualRule(${index})" title="Usar esta regla">
                    <i class="fas fa-edit"></i>
                </button>
                <button class="btn btn-outline btn-sm" onclick="deleteRule(${index})" title="Eliminar regla">
                    <i class="fas fa-trash"></i>
                </button>
            </div>
        </div>
        
        <div class="rule-card-content">
            <div class="rule-code-full">${formatRuleCode(rule.code)}</div>
            
            <div class="rule-meta">
                <div class="rule-timestamp">
                    <i class="fas fa-clock"></i>
                    ${formatTimestamp(rule.timestamp)}
                </div>
                <span class="rule-type">${rule.type || 'individual'}</span>
            </div>
        </div>
        
        <div class="rule-card-footer">
            <button class="btn-execute-rule" onclick="executeSingleRuleFromCard('${rule.code.replace(/'/g, "\\'")}', ${index})" 
                    data-rule-index="${index}" id="executeBtn-${index}">
                <i class="fas fa-play"></i>
                Ejecutar Esta Regla
            </button>
            
            <button class="btn btn-outline btn-sm" onclick="analyzeRule('${rule.code.replace(/'/g, "\\'")}')" title="Analizar regla">
                <i class="fas fa-search"></i> Analizar
            </button>
        </div>
    `;

  return card;
}

// 🔥 NUEVA FUNCIÓN: Formatear código de regla MEJORADO
function formatRuleCode(code) {
  if (!code) return '<span class="text-muted">Sin código</span>';

  // Resaltar sintaxis Prolog mejorada
  let formattedCode = code
    .replace(/%[^\n]*/g, '<span class="comment">$&</span>') // Comentarios
    .replace(/(:-)/g, '<span class="operator">$1</span>') // Operador :-
    .replace(/([A-Z][a-zA-Z0-9_]*)/g, '<span class="variable">$1</span>') // Variables
    .replace(/([a-z][a-zA-Z0-9_]*)(?=\()/g, '<span class="predicate">$1</span>') // Predicados
    .replace(/'([^']*)'/g, '<span class="string">\'$1\'</span>') // Strings
    .replace(/(\b\d+\.?\d*\b)/g, '<span class="number">$1</span>') // Números
    .replace(/(\.[\s]*$)/g, '<span class="operator">$1</span>'); // Punto final

  return formattedCode;
}

// 🔥 FUNCIÓN CORREGIDA: Ejecutar desde cards del carrusel
async function executeSingleRuleFromCard(ruleCode, ruleIndex) {
    const executeBtn = document.getElementById(`executeBtn-${ruleIndex}`);
    if (!executeBtn) return;

    // Extraer el nombre de la regla para usarlo como consulta
    const ruleName = extractRuleName(ruleCode);
    
    if (!ruleName) {
        showNotification('error', 'Error en regla', 'No se pudo extraer el nombre de la regla');
        return;
    }

    // Cambiar estado del botón
    executeBtn.disabled = true;
    executeBtn.classList.add('executing');
    executeBtn.innerHTML = '<i class="fas fa-spinner fa-spin"></i> Ejecutando...';

    try {
        console.log(`🚀 Ejecutando regla: ${ruleName}`);
        
        // Usar el nombre de la regla como consulta
        const result = await executeSingleQueryHybrid(ruleName + '.');
        
        // Mostrar notificación con resultado
        showSingleRuleNotification(ruleName, result, ruleIndex);

    } catch (error) {
        console.error('❌ Error ejecutando regla:', error);
        showNotification('error', 'Error en ejecución', 
            `No se pudo ejecutar la regla: ${error.message}`);
    } finally {
        // Restaurar botón
        executeBtn.disabled = false;
        executeBtn.classList.remove('executing');
        executeBtn.innerHTML = '<i class="fas fa-play"></i> Ejecutar Esta Regla';
    }
}

// 🔥 FUNCIÓN: Extraer nombre de regla
function extractRuleName(ruleCode) {
    if (!ruleCode) return null;
    
    // Buscar patrones comunes de definición de reglas
    const patterns = [
        /^([a-z][a-zA-Z0-9_]*)\s*:-/,  // regla(X) :- 
        /^([a-z][a-zA-Z0-9_]*)\s*\(/,   // regla(
        /^([a-z][a-zA-Z0-9_]*)\s*\./    // regla.
    ];
    
    for (const pattern of patterns) {
        const match = ruleCode.match(pattern);
        if (match && match[1]) {
            return match[1];
        }
    }
    
    return null;
}
// 🔥 FUNCIÓN: Configurar consultas de ejemplo CORRECTAS
function setupQueryExamples() {
    const examples = [
        {
            name: "Objetos detectados",
            query: "objeto_detectado(ID, Objeto, Confianza).",
            description: "Todos los objetos detectados en la imagen"
        },
        {
            name: "Objetos principales", 
            query: "objeto_principal(ID).",
            description: "Objetos con alta confianza"
        },
        {
            name: "Escena natural",
            query: "escena_natural.",
            description: "Verificar si la escena es natural"
        },
        {
            name: "Elementos peligrosos",
            query: "elemento_peligroso(ID).",
            description: "Objetos de alto riesgo"
        },
        {
            name: "Características",
            query: "caracteristica_observable(ID, Caracteristica).",
            description: "Todas las características detectadas"
        },
        {
            name: "Consulta básica",
            query: "member(X, [1,2,3]).",
            description: "Prueba básica del sistema"
        }
    ];

    const quickQueriesContainer = document.getElementById('quickQueries');
    if (!quickQueriesContainer) return;

    let html = `
        <div class="quick-queries">
            <h4><i class="fas fa-bolt"></i> Consultas Rápidas</h4>
            <div class="quick-query-buttons">
    `;

    examples.forEach(example => {
        html += `
            <button class="btn btn-outline btn-sm quick-query-btn" 
                    onclick="loadQueryExample('${example.query}', '${example.description}')"
                    title="${example.description}">
                <i class="fas fa-play-circle"></i> ${example.name}
            </button>
        `;
    });

    html += `
            </div>
            <small class="query-help">
                <i class="fas fa-info-circle"></i>
                Estas consultas usan los datos generados automáticamente
            </small>
        </div>
    `;

    quickQueriesContainer.innerHTML = html;
}

// 🔥 FUNCIÓN: Cargar ejemplo en el editor
function loadQueryExample(query, description) {
    const queryEditor = document.getElementById('prologQuery');
    if (queryEditor) {
        queryEditor.value = query;
        queryEditor.focus();
        
        showNotification('info', 'Consulta cargada', 
            `"${description}" - Modifícala si es necesario y ejecuta`);
    }
}
// 🔥 PRUEBA MÍNIMA DEL SISTEMA
async function testMinimalProlog() {
  showLoading('analyzing', 'Ejecutando prueba mínima...');

  const testQueries = [
    'member(a, [a,b,c]).',
    'escena_interior.',
    'objeto_detectado(X, Y, Z).'
  ];

  const results = [];

  for (const query of testQueries) {
    try {
      const result = await executeSingleQueryHybrid(query);
      results.push({
        query,
        success: result.success,
        count: result.count,
        source: result.source || 'server',
        error: result.error
      });
    } catch (error) {
      results.push({
        query,
        success: false,
        error: error.message
      });
    }
    await new Promise(resolve => setTimeout(resolve, 500));
  }

  hideLoading();

  // Mostrar resultados
  const notification = document.createElement('div');
  notification.className = 'notification info';
  notification.innerHTML = `
        <div class="notification-progress info"></div>
        <div class="notification-header">
            <div class="notification-icon">
                <i class="fas fa-vial"></i>
            </div>
            <div class="notification-content">
                <h4 class="notification-title">Prueba Mínima del Sistema</h4>
                <div class="notification-details">
                    ${results.map(result => `
                        <div class="detail-item">
                            <div class="test-result">
                                <span class="test-name">${result.query}</span>
                                <span class="test-status ${result.success ? 'success-text' : 'error-text'}">
                                    ${result.success ? '✅' : '❌'} 
                                    ${result.success ? `${result.count} resultados` : 'Error'}
                                    ${result.source ? ` (${result.source})` : ''}
                                </span>
                            </div>
                            ${result.error ? `
                                <div class="error-text">${result.error}</div>
                            ` : ''}
                        </div>
                    `).join('')}
                </div>
            </div>
            <button class="notification-close" onclick="this.parentElement.parentElement.remove()">
                <i class="fas fa-times"></i>
            </button>
        </div>
    `;

  const container = document.getElementById('notificationContainer') || createNotificationContainer();
  container.appendChild(notification);
}

// 🔥 CREAR CONTENEDOR DE NOTIFICACIONES SI NO EXISTE
function createNotificationContainer() {
  const container = document.createElement('div');
  container.id = 'notificationContainer';
  container.className = 'notification-container';
  document.body.appendChild(container);
  return container;
}

// 🔥 AGREGAR BOTONES DE DIAGNÓSTICO URGENTE
function addUrgentDiagnosticButtons() {
  const header = document.querySelector('header .header-content');
  if (header) {
    const urgentDiv = document.createElement('div');
    urgentDiv.style.display = 'flex';
    urgentDiv.style.gap = '10px';
    urgentDiv.style.alignItems = 'center';

    urgentDiv.innerHTML = `
            <button class="btn btn-sm btn-danger" onclick="urgentServerDiagnostic()">
                <i class="fas fa-exclamation-triangle"></i> Diagnóstico Urgente
            </button>
            <button class="btn btn-sm btn-warning" onclick="testMinimalProlog()">
                <i class="fas fa-vial"></i> Prueba Rápida
            </button>
            <span class="mode-indicator" id="modeIndicator" style="
                background: #ff6b6b; 
                color: white; 
                padding: 4px 8px; 
                border-radius: 12px; 
                font-size: 0.8rem;
                font-weight: bold;
            ">🚨 MODO RESCATE</span>
        `;

    header.appendChild(urgentDiv);
  }
}

// 🔥 FUNCIÓN MEJORADA: Validar consultas Prolog
function validatePrologQuery(query) {
    if (!query || query.trim().length === 0) {
        return { isValid: false, error: 'La consulta no puede estar vacía' };
    }

    const trimmedQuery = query.trim();

    // Debe terminar con punto
    if (!trimmedQuery.endsWith('.')) {
        return { isValid: false, error: 'La consulta debe terminar con un punto (.)' };
    }

    // No debe contener el operador :- (definición de reglas)
    if (trimmedQuery.includes(':-')) {
        return { 
            isValid: false, 
            error: 'Esto parece una definición de regla. Para definir reglas, usa el editor de reglas. Para consultar, usa solo el objetivo. Ejemplo: en lugar de "mi_regla(X) :- condicion(X).", usa "mi_regla(X)."' 
        };
    }

    // Longitud razonable
    if (trimmedQuery.length > 500) {
        return { isValid: false, error: 'La consulta es demasiado larga' };
    }

    return { isValid: true, error: null };
}

// 🔥 DIAGNÓSTICO DEL SERVIDOR
async function diagnoseServer() {
  showLoading('analyzing', 'Diagnosticando servidor...');

  const endpoints = [
    { path: '/api/status', method: 'GET', name: 'Estado del API' },
    { path: '/query/prolog/simple', method: 'POST', name: 'Endpoint Simple' },
    { path: '/query/prolog', method: 'POST', name: 'Endpoint Principal' }
  ];

  const results = [];

  for (const endpoint of endpoints) {
    try {
      console.log(`🔍 Probando: ${endpoint.name}`);

      const options = {
        method: endpoint.method,
        headers: {
          'Content-Type': 'application/json'
        }
      };

      if (endpoint.method === 'POST') {
        options.body = JSON.stringify({
          query: 'member(X, [1,2,3]).',
          sessionId: appState.sessionId
        });
      }

      const response = await fetch(endpoint.path, options);
      const contentType = response.headers.get('content-type');
      const isJson = contentType && contentType.includes('application/json');

      let data;
      if (isJson) {
        data = await response.json();
      } else {
        data = { htmlResponse: true, status: response.status };
      }

      results.push({
        name: endpoint.name,
        path: endpoint.path,
        status: response.status,
        ok: response.ok,
        isJson: isJson,
        data: data
      });

    } catch (error) {
      results.push({
        name: endpoint.name,
        path: endpoint.path,
        status: 'ERROR',
        ok: false,
        error: error.message
      });
    }

    await new Promise(resolve => setTimeout(resolve, 500));
  }

  hideLoading();
  showServerDiagnostic(results);
}

// 🔥 MOSTRAR DIAGNÓSTICO DEL SERVIDOR
function showServerDiagnostic(results) {
  const notification = document.createElement('div');
  notification.className = 'notification info';
  notification.innerHTML = `
        <div class="notification-progress info"></div>
        <div class="notification-header">
            <div class="notification-icon">
                <i class="fas fa-server"></i>
            </div>
            <div class="notification-content">
                <h4 class="notification-title">Diagnóstico del Servidor</h4>
                <div class="notification-details">
                    ${results.map(result => `
                        <div class="detail-item">
                            <div class="test-result">
                                <span class="test-name">${result.name}</span>
                                <span class="test-status ${result.ok ? 'success-text' : 'error-text'}">
                                    ${result.ok ? '✅' : '❌'} ${result.status}
                                </span>
                            </div>
                            <div class="endpoint-info">
                                <code>${result.method || 'GET'} ${result.path}</code>
                                ${result.isJson === false ? '<span class="warning-text">⚠️ No devuelve JSON</span>' : ''}
                                ${result.error ? `<div class="error-text">${result.error}</div>` : ''}
                            </div>
                        </div>
                    `).join('')}
                </div>
            </div>
            <button class="notification-close" onclick="this.parentElement.parentElement.remove()">
                <i class="fas fa-times"></i>
            </button>
        </div>
        <div class="notification-actions">
            <button class="btn btn-sm btn-primary" onclick="diagnoseServer()">
                <i class="fas fa-redo"></i> Re-diagnosticar
            </button>
            <button class="btn btn-sm btn-outline" onclick="showServerFixes()">
                <i class="fas fa-tools"></i> Soluciones
            </button>
        </div>
    `;

  const container = document.getElementById('notificationContainer');
  if (!container) {
    const newContainer = document.createElement('div');
    newContainer.id = 'notificationContainer';
    newContainer.className = 'notification-container';
    document.body.appendChild(newContainer);
  }
  container.appendChild(notification);
}

// 🔥 MOSTRAR SOLUCIONES PARA EL SERVIDOR
function showServerFixes() {
  const notification = document.createElement('div');
  notification.className = 'notification warning';
  notification.innerHTML = `
        <div class="notification-progress warning"></div>
        <div class="notification-header">
            <div class="notification-icon">
                <i class="fas fa-tools"></i>
            </div>
            <div class="notification-content">
                <h4 class="notification-title">Soluciones para el Servidor</h4>
                <div class="notification-details">
                    <div class="detail-item">
                        <strong>Problema:</strong> La ruta <code>/query/prolog/simple</code> tiene un error
                    </div>
                    <div class="detail-item">
                        <strong>Solución 1:</strong> Edita <code>server.js</code> y busca la ruta <code>/query/prolog/simple</code>
                    </div>
                    <div class="detail-item">
                        <strong>Solución 2:</strong> Asegúrate de que tenga:<br>
                        <code>const { query, sessionId = 'default' } = req.body;</code>
                    </div>
                    <div class="detail-item">
                        <strong>Solución 3:</strong> Reinicia el servidor después de hacer cambios
                    </div>
                </div>
            </div>
            <button class="notification-close" onclick="this.parentElement.parentElement.remove()">
                <i class="fas fa-times"></i>
            </button>
        </div>
    `;

  const container = document.getElementById('notificationContainer');
  container.appendChild(notification);
}


// 🔥 FUNCIÓN DE DIAGNÓSTICO COMPLETO
async function comprehensiveDiagnostic() {
  showLoading('analyzing', 'Realizando diagnóstico completo...');

  const tests = [
    { name: 'Conexión servidor', test: testServerConnection },
    { name: 'SWI-Prolog', test: testPrologConnection },
    { name: 'Consultas básicas', test: testBasicQueries },
    { name: 'Reglas guardadas', test: testSavedRules },
    { name: 'Sistema de imágenes', test: testImageSystem }
  ];

  const results = [];

  for (const test of tests) {
    try {
      console.log(`🧪 Ejecutando: ${test.name}`);
      const result = await test.test();
      results.push({
        name: test.name,
        success: true,
        result: result
      });
    } catch (error) {
      console.error(`❌ Falló: ${test.name}`, error);
      results.push({
        name: test.name,
        success: false,
        error: error.message
      });
    }
    await new Promise(resolve => setTimeout(resolve, 500));
  }

  hideLoading();
  showDiagnosticResults(results);
}

// Agregar botones de diagnóstico
function addDiagnosticButtons() {
  const querySection = document.querySelector('.query-suggestions');
  if (querySection) {
    const diagnosticDiv = document.createElement('div');
    diagnosticDiv.className = 'diagnostic-buttons';
    diagnosticDiv.style.display = 'flex';
    diagnosticDiv.style.gap = '10px';
    diagnosticDiv.style.marginTop = '15px';
    diagnosticDiv.style.flexWrap = 'wrap';

    diagnosticDiv.innerHTML = `
            <button class="btn btn-outline btn-sm" onclick="diagnoseServer()">
                <i class="fas fa-server"></i> Diagnosticar Servidor
            </button>
            <button class="btn btn-outline btn-sm" onclick="comprehensiveDiagnostic()">
                <i class="fas fa-stethoscope"></i> Diagnóstico Completo
            </button>
            <button class="btn btn-outline btn-sm" onclick="testBasicProlog()">
                <i class="fas fa-vial"></i> Probar Prolog Básico
            </button>
        `;

    querySection.appendChild(diagnosticDiv);
  }
}

// 🔥 PRUEBA BÁSICA DE PROLOG
async function testBasicProlog() {
  const testQueries = [
    'member(a, [a,b,c]).',
    'length([1,2,3], L).',
    'X is 2 + 2.',
    'append([1,2], [3,4], R).'
  ];

  showLoading('querying', 'Probando consultas básicas...');

  const results = [];
  for (const query of testQueries) {
    try {
      const result = await executeSingleQuery(query);
      results.push({
        query,
        success: result.success,
        count: result.count,
        error: result.error
      });
    } catch (error) {
      results.push({
        query,
        success: false,
        error: error.message
      });
    }
    await new Promise(resolve => setTimeout(resolve, 300));
  }

  hideLoading();
  showTestResults(results);
}

// 🔥 FUNCIONES DE PRUEBA INDIVIDUALES
async function testServerConnection() {
  const response = await fetch('/api/status');
  if (!response.ok) throw new Error(`HTTP ${response.status}`);
  const data = await response.json();
  return data;
}

async function testPrologConnection() {
  const testQueries = [
    'member(X, [1,2,3]).',
    'length([a,b,c], L).',
    'X is 2 + 2.'
  ];

  const results = [];
  for (const query of testQueries) {
    const result = await executeSingleQuery(query);
    results.push({ query, success: result.success });
  }
  return results;
}

async function testBasicQueries() {
  const queries = [
    'objeto_detectado(X, Y, Z).',
    'nivel_riesgo(X, bajo).',
    'caracteristica_observable(X, C).'
  ];

  const results = [];
  for (const query of queries) {
    try {
      const result = await executeSingleQuery(query);
      results.push({
        query,
        success: result.success,
        count: result.count,
        error: result.error
      });
    } catch (error) {
      results.push({
        query,
        success: false,
        error: error.message
      });
    }
  }
  return results;
}

async function testSavedRules() {
  // Verificar si hay reglas guardadas
  const session = sessionData.get(appState.sessionId);
  const hasRules = session && session.savedRules && Object.keys(session.savedRules).length > 0;
  return { hasSavedRules: hasRules };
}

async function testImageSystem() {
  // Verificar análisis de imagen
  const session = sessionData.get(appState.sessionId);
  const hasImageAnalysis = session && session.imageAnalysis;
  return { hasImageAnalysis: hasImageAnalysis };
}

// 🔥 MOSTRAR RESULTADOS DEL DIAGNÓSTICO
function showDiagnosticResults(results) {
  const passed = results.filter(r => r.success).length;
  const total = results.length;

  const notification = document.createElement('div');
  notification.className = `notification ${passed === total ? 'success' : passed > total / 2 ? 'warning' : 'error'}`;

  let resultsHTML = '';
  results.forEach(result => {
    resultsHTML += `
            <div class="detail-item">
                <div class="test-result">
                    <span class="test-name">${result.name}</span>
                    <span class="test-status ${result.success ? 'success-text' : 'error-text'}">
                        ${result.success ? '✅' : '❌'} ${result.success ? 'ÉXITO' : 'FALLO'}
                    </span>
                </div>
                ${!result.success ? `
                    <div class="test-error">${result.error}</div>
                ` : ''}
            </div>
        `;
  });

  notification.innerHTML = `
        <div class="notification-progress ${passed === total ? 'success' : passed > total / 2 ? 'warning' : 'error'}"></div>
        <div class="notification-header">
            <div class="notification-icon">
                <i class="fas ${passed === total ? 'fa-check-circle' : passed > total / 2 ? 'fa-exclamation-triangle' : 'fa-exclamation-circle'}"></i>
            </div>
            <div class="notification-content">
                <h4 class="notification-title">
                    Diagnóstico del Sistema: ${passed}/${total} Pruebas Exitosas
                </h4>
                <div class="notification-message">
                    ${passed === total ?
      '✅ Todos los sistemas funcionan correctamente' :
      passed > total / 2 ?
        '⚠️ Algunos sistemas necesitan atención' :
        '❌ Múltiples sistemas presentan problemas'
    }
                </div>
                <div class="notification-details">
                    ${resultsHTML}
                </div>
            </div>
            <button class="notification-close" onclick="this.parentElement.parentElement.remove()">
                <i class="fas fa-times"></i>
            </button>
        </div>
        <div class="notification-actions">
            <button class="btn btn-sm btn-primary" onclick="comprehensiveDiagnostic()">
                <i class="fas fa-redo"></i> Ejecutar Nuevamente
            </button>
            <button class="btn btn-sm btn-outline" onclick="showTroubleshootingGuide()">
                <i class="fas fa-life-ring"></i> Guía de Solución
            </button>
        </div>
    `;

  const container = document.getElementById('notificationContainer');
  if (!container) {
    const newContainer = document.createElement('div');
    newContainer.id = 'notificationContainer';
    newContainer.className = 'notification-container';
    document.body.appendChild(newContainer);
    container = newContainer;
  }

  container.appendChild(notification);
}

// 🔥 FUNCIÓN MEJORADA: Mostrar notificación para regla individual
function showSingleRuleNotification(ruleCode, result, ruleIndex) {
  const ruleName = ruleCode.split(':-')[0]?.split('(')[0]?.trim() || `Regla ${ruleIndex + 1}`;
  const isSuccess = result.success;
  const resultCount = result.count || 0;
  const errorMessage = result.error || 'Error desconocido';

  // Crear notificación mejorada
  const notification = document.createElement('div');
  notification.className = `notification ${isSuccess ? 'success' : 'error'} floating-notification`;
  notification.innerHTML = `
        <div class="notification-progress ${isSuccess ? 'success' : 'error'}"></div>
        <div class="notification-header">
            <div class="notification-icon">
                <i class="fas ${isSuccess ? 'fa-check-circle' : 'fa-exclamation-circle'}"></i>
            </div>
            <div class="notification-content">
                <h4 class="notification-title">
                    ${isSuccess ? '✅ ÉXITO: ' : '❌ ERROR: '}${ruleName}
                </h4>
                <div class="notification-message">
                    ${isSuccess ?
      `La regla se ejecutó correctamente` :
      `Error en la ejecución`
    }
                </div>
                <div class="notification-details">
                    <div class="detail-item">
                        <strong>Regla:</strong> 
                        <code class="rule-code">${ruleCode}</code>
                    </div>
                    <div class="detail-item">
                        <strong>Resultados:</strong> 
                        <span class="${isSuccess ? 'success-text' : 'error-text'}">
                            ${isSuccess ?
      `${resultCount} resultado(s) encontrado(s)` :
      errorMessage
    }
                        </span>
                    </div>
                    ${isSuccess && resultCount > 0 ? `
                        <div class="detail-item">
                            <strong>Datos:</strong>
                            <pre class="result-data">${JSON.stringify(result.results || [], null, 2)}</pre>
                        </div>
                    ` : ''}
                </div>
            </div>
            <button class="notification-close" onclick="this.parentElement.parentElement.remove()">
                <i class="fas fa-times"></i>
            </button>
        </div>
        ${isSuccess && resultCount > 0 ? `
            <div class="notification-actions">
                <button class="btn btn-sm btn-outline" onclick="exportRuleResults(${ruleIndex})">
                    <i class="fas fa-download"></i> Exportar Resultados
                </button>
                <button class="btn btn-sm btn-primary" onclick="showRuleAnalysis('${ruleCode.replace(/'/g, "\\'")}')">
                    <i class="fas fa-chart-bar"></i> Análisis Detallado
                </button>
            </div>
        ` : ''}
    `;

  const container = document.getElementById('notificationContainer');
  if (!container) {
    // Crear contenedor si no existe
    const newContainer = document.createElement('div');
    newContainer.id = 'notificationContainer';
    newContainer.className = 'notification-container';
    document.body.appendChild(newContainer);
    container = newContainer;
  }

  container.appendChild(notification);

  // Auto-remover después de 10 segundos (más tiempo para leer)
  setTimeout(() => {
    if (notification.parentElement) {
      notification.style.animation = 'slideOutRight 0.5s ease-in forwards';
      setTimeout(() => {
        if (notification.parentElement) {
          notification.remove();
        }
      }, 500);
    }
  }, 10000);
}

// 🔥 NUEVA FUNCIÓN: Copiar regla al portapapeles
function copyRuleToClipboard(ruleCode) {
  navigator.clipboard.writeText(ruleCode).then(() => {
    showNotification('success', 'Copiada', 'Regla copiada al portapapeles');
  }).catch(err => {
    showNotification('error', 'Error', 'No se pudo copiar la regla');
  });
}

// 🔥 NUEVA FUNCIÓN: Usar regla individual en el editor
function editIndividualRule(ruleIndex) {
  const rule = carouselState.currentRuleSet[ruleIndex];
  if (rule && rule.code) {
    // Agregar la regla al editor principal
    addToCustomRules(rule.code);

    showNotification('info', 'Regla Agregada',
      `"${rule.name}" ha sido agregada al editor de reglas`);
  }
}

// 🔥 NUEVA FUNCIÓN: Analizar regla
function analyzeRule(ruleCode) {
  // Extraer información de la regla para análisis
  const analysis = {
    type: ruleCode.includes(':-') ? 'regla_compleja' : 'hecho_simple',
    hasVariables: /[A-Z]/.test(ruleCode),
    length: ruleCode.length,
    complexity: ruleCode.split(',').length // Número de condiciones
  };

  let analysisText = `Análisis de la regla:\n`;
  analysisText += `- Tipo: ${analysis.type}\n`;
  analysisText += `- Variables: ${analysis.hasVariables ? 'Sí' : 'No'}\n`;
  analysisText += `- Longitud: ${analysis.length} caracteres\n`;
  analysisText += `- Complejidad: ${analysis.complexity} condición(es)`;

  showNotification('info', 'Análisis de Regla', analysisText);
}

// 🔥 NUEVA FUNCIÓN: Mostrar resultados detallados
function showDetailedResults(ruleIndex) {
  const rule = carouselState.currentRuleSet[ruleIndex];
  if (!rule) return;

  // Aquí podrías abrir un modal o expandir la card con resultados detallados
  const card = document.querySelector(`[data-rule-index="${ruleIndex}"]`)?.closest('.rule-card');
  if (card) {
    card.classList.toggle('expanded');
  }

  showNotification('info', 'Resultados Detallados',
    `Funcionalidad de resultados detallados para "${rule.name}"`);
}

// 🔥 NUEVA FUNCIÓN: Formatear timestamp
function formatTimestamp(timestamp) {
  if (!timestamp) return 'Recién guardada';

  const date = new Date(timestamp);
  const now = new Date();
  const diffMs = now - date;
  const diffMins = Math.floor(diffMs / 60000);
  const diffHours = Math.floor(diffMs / 3600000);
  const diffDays = Math.floor(diffMs / 86400000);

  if (diffMins < 1) return 'Ahora mismo';
  if (diffMins < 60) return `Hace ${diffMins} min`;
  if (diffHours < 24) return `Hace ${diffHours} h`;
  if (diffDays < 7) return `Hace ${diffDays} d`;

  return date.toLocaleDateString();
}

// 🔥 NUEVA FUNCIÓN: Ejecutar regla desde card
async function executeRuleFromCard(ruleIndex) {
  const rule = carouselState.currentRuleSet[ruleIndex];
  if (!rule || !rule.code) {
    showNotification('error', 'Error', 'La regla no tiene código válido');
    return;
  }

  const executeBtn = document.getElementById(`executeBtn-${ruleIndex}`);
  if (!executeBtn) return;

  // Cambiar estado del botón
  executeBtn.disabled = true;
  executeBtn.classList.add('executing');
  executeBtn.innerHTML = '<i class="fas fa-spinner fa-spin"></i> Ejecutando...';

  try {
    // Extraer consultas individuales del código
    const queries = extractQueriesFromRule(rule.code);

    if (queries.length === 0) {
      throw new Error('No se encontraron consultas válidas en la regla');
    }

    // Ejecutar cada consulta
    const results = [];
    for (let i = 0; i < queries.length; i++) {
      const query = queries[i];
      const result = await executeSingleQuery(query);
      results.push({
        query: query,
        result: result
      });

      // Pequeña pausa entre consultas
      await new Promise(resolve => setTimeout(resolve, 100));
    }

    // Mostrar notificación con resultados
    showRuleExecutionNotification(rule.name || `Regla ${ruleIndex + 1}`, results);

  } catch (error) {
    showNotification('error', 'Error en ejecución',
      `No se pudo ejecutar la regla: ${error.message}`);
  } finally {
    // Restaurar botón
    executeBtn.disabled = false;
    executeBtn.classList.remove('executing');
    executeBtn.innerHTML = '<i class="fas fa-play"></i> Ejecutar Consulta';
  }
}

// 🔥 NUEVA FUNCIÓN: Extraer consultas de una regla
function extractQueriesFromRule(ruleCode) {
  const lines = ruleCode.split('\n')
    .map(line => line.trim())
    .filter(line => line && !line.startsWith('%'));

  const queries = [];

  lines.forEach(line => {
    // Buscar consultas (líneas que terminan en punto y no son reglas)
    if (line.endsWith('.') && !line.includes(':-')) {
      // Remover el punto final temporalmente
      const query = line.substring(0, line.length - 1).trim();
      if (query && !query.startsWith('%')) {
        queries.push(query + '.');
      }
    }

    // También buscar consultas dentro de reglas
    if (line.includes(':-')) {
      const parts = line.split(':-');
      if (parts.length > 1) {
        const body = parts[1].trim();
        if (body.endsWith('.')) {
          const bodyQuery = body.substring(0, body.length - 1).trim();
          if (bodyQuery && !bodyQuery.startsWith('%')) {
            queries.push(bodyQuery + '.');
          }
        }
      }
    }
  });

  // Si no se encontraron consultas específicas, usar la primera línea que parezca una consulta
  if (queries.length === 0) {
    const firstMeaningfulLine = lines.find(line =>
      line.endsWith('.') && line.length > 3 && !line.startsWith('%')
    );
    if (firstMeaningfulLine) {
      queries.push(firstMeaningfulLine);
    }
  }

  return queries.length > 0 ? queries : ['member(X, [1,2,3]).']; // Consulta de fallback
}

// 🔥 FUNCIÓN MEJORADA: Ejecutar consulta simple con fallback
async function executeSingleQuery(query) {
  try {
    console.log(`📤 Enviando consulta: ${query}`);

    // PRIMERO intentar con la ruta simple
    try {
      const response = await fetch('/query/prolog/simple', {
        method: 'POST',
        headers: {
          'Content-Type': 'application/json'
        },
        body: JSON.stringify({
          query: query,
          sessionId: appState.sessionId
        })
      });

      const contentType = response.headers.get('content-type');
      if (contentType && contentType.includes('application/json')) {
        const result = await response.json();
        if (response.ok) {
          return result;
        }
      }
    } catch (simpleError) {
      console.log('🔄 Ruta simple falló, intentando con ruta principal...');
    }

    // FALLBACK: usar la ruta principal
    const response = await fetch('/query/prolog', {
      method: 'POST',
      headers: {
        'Content-Type': 'application/json'
      },
      body: JSON.stringify({
        query: query,
        sessionId: appState.sessionId,
        useSavedRules: false,
        customRules: ''
      })
    });

    const contentType = response.headers.get('content-type');
    if (!contentType || !contentType.includes('application/json')) {
      const textResponse = await response.text();
      console.error('❌ El servidor devolvió HTML:', textResponse.substring(0, 200));
      throw new Error(`Error del servidor: ${response.status}`);
    }

    const result = await response.json();

    if (!response.ok) {
      throw new Error(result.error || `Error ${response.status}`);
    }

    return result;
  } catch (error) {
    console.error('❌ Error en executeSingleQuery:', error);
    return {
      success: false,
      error: error.message,
      results: [],
      count: 0
    };
  }
}

// 🔥 NUEVA FUNCIÓN: Mostrar notificación de ejecución
function showRuleExecutionNotification(ruleName, results) {
  const totalQueries = results.length;
  const successfulQueries = results.filter(r => r.result.success).length;
  const totalResults = results.reduce((sum, r) => sum + (r.result.count || 0), 0);

  let notificationType = 'success';
  let title = `✅ ${ruleName} ejecutada`;
  let message = `${successfulQueries}/${totalQueries} consultas exitosas`;

  if (successfulQueries === 0) {
    notificationType = 'error';
    title = `❌ ${ruleName} falló`;
    message = 'Ninguna consulta se ejecutó correctamente';
  } else if (successfulQueries < totalQueries) {
    notificationType = 'warning';
    title = `⚠️ ${ruleName} parcialmente exitosa`;
    message = `${successfulQueries}/${totalQueries} consultas exitosas`;
  }

  const notification = document.createElement('div');
  notification.className = `notification ${notificationType}`;
  notification.innerHTML = `
        <div class="notification-progress"></div>
        <div class="notification-header">
            <div class="notification-icon">
                <i class="fas ${notificationType === 'success' ? 'fa-check-circle' :
      notificationType === 'error' ? 'fa-exclamation-circle' :
        'fa-exclamation-triangle'
    }"></i>
            </div>
            <h4 class="notification-title">${title}</h4>
            <button class="notification-close" onclick="this.parentElement.parentElement.remove()">
                <i class="fas fa-times"></i>
            </button>
        </div>
        <div class="notification-content">
            <p><strong>${message}</strong> - ${totalResults} resultados totales</p>
            
            ${results.slice(0, 3).map((result, idx) => `
                <div class="notification-query">
                    <strong>Consulta ${idx + 1}:</strong> ${result.query}
                </div>
                <div class="notification-results">
                    <strong>Resultados (${result.result.count || 0}):</strong>\n${result.result.success ?
        JSON.stringify(result.result.results || [], null, 2) :
        `Error: ${result.result.error || 'Desconocido'}`
      }
                </div>
            `).join('')}
            
            ${results.length > 3 ? `
                <p class="text-muted">... y ${results.length - 3} consultas más</p>
            ` : ''}
        </div>
    `;

  const container = document.getElementById('notificationContainer');
  container.appendChild(notification);

  // Auto-remover después de 5 segundos
  setTimeout(() => {
    if (notification.parentElement) {
      notification.remove();
    }
  }, 5000);
}

// 🔥 NUEVA FUNCIÓN: Sistema de notificaciones genérico
function showNotification(type, title, message, details = null) {
  const notification = document.createElement('div');
  notification.className = `notification ${type}`;
  notification.innerHTML = `
        <div class="notification-progress"></div>
        <div class="notification-header">
            <div class="notification-icon">
                <i class="fas ${type === 'success' ? 'fa-check-circle' :
      type === 'error' ? 'fa-exclamation-circle' :
        type === 'warning' ? 'fa-exclamation-triangle' :
          'fa-info-circle'
    }"></i>
            </div>
            <h4 class="notification-title">${title}</h4>
            <button class="notification-close" onclick="this.parentElement.parentElement.remove()">
                <i class="fas fa-times"></i>
            </button>
        </div>
        <div class="notification-content">
            <p>${message}</p>
            ${details ? `<div class="notification-results">${details}</div>` : ''}
        </div>
    `;

  const container = document.getElementById('notificationContainer');
  container.appendChild(notification);

  setTimeout(() => {
    if (notification.parentElement) {
      notification.remove();
    }
  }, 5000);
}

// 🔥 NUEVA FUNCIÓN: Navegación del carrusel
function nextRuleCard() {
  const maxIndex = carouselState.currentRuleSet.length - carouselState.rulesPerView;
  if (carouselState.currentIndex < maxIndex) {
    carouselState.currentIndex += carouselState.rulesPerView;
    updateCarousel();
    updateCarouselInfo();
  }
}

function prevRuleCard() {
  if (carouselState.currentIndex > 0) {
    carouselState.currentIndex -= carouselState.rulesPerView;
    updateCarousel();
    updateCarouselInfo();
  }
}

function goToPage(pageIndex) {
  carouselState.currentIndex = pageIndex * carouselState.rulesPerView;
  updateCarousel();
  updateCarouselInfo();
}

function updateCarouselInfo() {
  const infoElement = document.getElementById('carouselInfo');
  if (infoElement) {
    const totalRules = carouselState.currentRuleSet.length;
    const currentStart = carouselState.currentIndex + 1;
    const currentEnd = Math.min(
      carouselState.currentIndex + carouselState.rulesPerView,
      totalRules
    );
    infoElement.textContent = `Mostrando ${currentStart}-${currentEnd} de ${totalRules} reglas`;
  }
}


function setupAutonomousMode() {
  const modeIndicator = document.createElement('div');
  modeIndicator.className = 'autonomous-mode-indicator';
  modeIndicator.innerHTML = `
        <div class="mode-badge">
            <i class="fas fa-robot"></i>
            MODO AUTÓNOMO ACTIVO
        </div>
    `;

  const header = document.querySelector('header');
  header.appendChild(modeIndicator);
}

function initializeEventListeners() {
  // Drag and drop para datos
  const dataUploadArea = document.getElementById('dataUploadArea');
  const imageUploadArea = document.getElementById('imageUploadArea');

  if (dataUploadArea) {
    dataUploadArea.addEventListener('dragover', handleDragOver);
    dataUploadArea.addEventListener('dragleave', handleDragLeave);
    dataUploadArea.addEventListener('drop', (e) => handleDrop(e, 'data'));
  }

  if (imageUploadArea) {
    imageUploadArea.addEventListener('dragover', handleDragOver);
    imageUploadArea.addEventListener('dragleave', handleDragLeave);
    imageUploadArea.addEventListener('drop', (e) => handleDrop(e, 'image'));
  }

  // File input change
  const dataFileInput = document.getElementById('dataFileInput');
  const imageFileInput = document.getElementById('imageFileInput');

  if (dataFileInput) {
    dataFileInput.addEventListener('change', (e) => handleFileSelect(e, 'data'));
  }

  if (imageFileInput) {
    imageFileInput.addEventListener('change', (e) => handleFileSelect(e, 'image'));
  }

  // // Enter key en consulta Prolog
  // prologQuery.addEventListener('keypress', function (e) {
  //     if (e.key === 'Enter' && e.ctrlKey) {
  //         executePrologQuery();
  //     }
  // });
}

// Manejo de archivos
function handleDragOver(e) {
  e.preventDefault();
  e.currentTarget.classList.add('dragover');
}

function handleDragLeave(e) {
  e.preventDefault();
  e.currentTarget.classList.remove('dragover');
}

function handleDrop(e, type) {
  e.preventDefault();
  e.currentTarget.classList.remove('dragover');

  const files = e.dataTransfer.files;
  if (files.length > 0) {
    if (type === 'data') {
      processDataFile(files[0]);
    } else {
      processImageFile(files[0]);
    }
  }
}

function handleFileSelect(e, type) {
  if (e.target.files.length > 0) {
    if (type === 'data') {
      processDataFile(e.target.files[0]);
    } else {
      processImageFile(e.target.files[0]);
    }
  }
}

// Actualizar processImageFile para ocultar 3D
async function processImageFile(file) {
    // Ocultar sección 3D para imágenes
    hide3DVisualizationSection();
    
    showLoading();

    const formData = new FormData();
    formData.append('image', file);
    formData.append('sessionId', appState.sessionId);

    try {
        const response = await fetch('/analyze/image/autonomous', {
            method: 'POST',
            body: formData
        });

        const result = await response.json();

        if (result.success) {
            showAlert(`🤖 ${result.message}`, 'success');
            displayAutonomousAnalysis(result.analysis, result.prologFacts);
            loadAutonomousQuerySuggestions(result.analysis);
        } else {
            throw new Error(result.error);
        }
    } catch (error) {
        showAlert(`❌ Error en análisis autónomo: ${error.message}`, 'danger');
    } finally {
        hideLoading();
    }
}


function displayAutonomousAnalysis(analysis, prologFacts) {
  const prevAnalysis = document.querySelector('.autonomous-analysis-results');
  if (prevAnalysis) {
    prevAnalysis.remove();
  }

  const resultsContainer = document.createElement('div');
  resultsContainer.className = 'autonomous-analysis-results';

  resultsContainer.innerHTML = `
    <div class="card">
      <div class="card-header">
        <div class="card-title">
          <i class="fas fa-brain"></i>
          Análisis Autónomo Generativo
          <div class="autonomous-badge">
            <i class="fas fa-magic"></i>
            CONOCIMIENTO GENERADO AUTOMÁTICAMENTE
          </div>
        </div>
      </div>
      
      <div class="analysis-sections">
        <!-- Resumen del Sistema Autónomo -->
        <div class="analysis-section">
          <h4><i class="fas fa-chart-pie"></i> Resumen del Análisis</h4>
          <div class="autonomous-stats">
            <div class="stat-card">
              <div class="stat-icon">
                <i class="fas fa-shapes"></i>
              </div>
              <div class="stat-info">
                <div class="stat-value">${analysis.autonomousClassification?.objectCount || 0}</div>
                <div class="stat-label">Objetos Identificados</div>
              </div>
            </div>
            <div class="stat-card">
              <div class="stat-icon">
                <i class="fas fa-tags"></i>
              </div>
              <div class="stat-info">
                <div class="stat-value">${analysis.autonomousClassification?.allCategories?.length || 0}</div>
                <div class="stat-label">Categorías Generadas</div>
              </div>
            </div>
            <div class="stat-card">
              <div class="stat-icon">
                <i class="fas fa-shield-alt"></i>
              </div>
              <div class="stat-info">
                <div class="stat-value">${analysis.safetyAssessment?.overallRisk || 'BAJO'}</div>
                <div class="stat-label">Riesgo Global</div>
              </div>
            </div>
            <div class="stat-card">
              <div class="stat-icon">
                <i class="fas fa-project-diagram"></i>
              </div>
              <div class="stat-info">
                <div class="stat-value">${analysis.autonomousClassification?.diversityIndex || '0'}</div>
                <div class="stat-label">Índice Diversidad</div>
              </div>
            </div>
          </div>
        </div>
        
        <!-- Descripción de la Escena -->
        <div class="analysis-section">
          <h4><i class="fas fa-globe-americas"></i> Descripción de la Escena</h4>
          <div class="scene-description">
            <div class="scene-card">
              <div class="scene-header">
                <i class="fas fa-camera"></i>
                <strong>Análisis Contextual</strong>
              </div>
              <div class="scene-content">
                <p><strong>Descripción IA:</strong> ${analysis.caption || 'No disponible'}</p>
                <p><strong>Tipo de Escena:</strong> ${analysis.characteristics?.escena_completa?.tipo_escena || 'No determinado'}</p>
                <p><strong>Ambiente:</strong> ${analysis.characteristics?.escena_completa?.ambiente || 'No determinado'}</p>
                <p><strong>Elementos Principales:</strong> ${analysis.characteristics?.escena_completa?.elementos_principales?.join(', ') || 'No identificados'}</p>
              </div>
            </div>
          </div>
        </div>
        
        <!-- Objetos y Características Generadas -->
        <div class="analysis-section">
          <h4><i class="fas fa-microscope"></i> Sistema de Características Generado</h4>
          <div class="generated-characteristics">
            ${Object.values(analysis.characteristics || {}).filter(char => char.id).map(char => `
              <div class="characteristic-card ${char.riesgo?.nivel?.toLowerCase() || 'bajo'}">
                <div class="char-header">
                  <div class="char-name">${char.nombre || 'Objeto sin nombre'}</div>
                  <div class="char-risk ${char.riesgo?.nivel?.toLowerCase() || 'bajo'}">
                    <i class="fas ${char.riesgo?.nivel === 'ALTO' ? 'fa-exclamation-triangle' : char.riesgo?.nivel === 'MEDIO' ? 'fa-info-circle' : 'fa-check-circle'}"></i>
                    ${char.riesgo?.nivel || 'BAJO'} (${char.riesgo?.puntuacion || 0})
                  </div>
                </div>
                <div class="char-classification">
                  <strong>Clasificación:</strong> ${char.clasificacion || 'No clasificado'}
                </div>
                <div class="char-features">
                  <div class="features-section">
                    <strong>Colores:</strong> ${char.colores?.join(', ') || 'No detectados'}
                  </div>
                  <div class="features-section">
                    <strong>Formas:</strong> ${char.formas?.join(', ') || 'No detectadas'}
                  </div>
                  <div class="features-section">
                    <strong>Texturas:</strong> ${char.texturas?.join(', ') || 'No detectadas'}
                  </div>
                </div>
                <div class="char-recommendations">
                  ${char.recomendaciones?.map(rec => `<div class="recommendation">${rec}</div>`).join('') || '<div class="recommendation">No se requieren acciones específicas</div>'}
                </div>
              </div>
            `).join('')}
          </div>
        </div>
        
        <!-- Sistema Experto Generado -->
        <div class="analysis-section">
          <h4><i class="fas fa-robot"></i> Sistema Experto Autónomo</h4>
          <div class="expert-system">
            <div class="expert-rules">
              <h5>Reglas de Inferencia Generadas:</h5>
              <div class="rules-list">
                ${analysis.expertSystem?.rules?.map(rule => `
                  <div class="rule-item">
                    <code>${rule}</code>
                  </div>
                `).join('') || '<div class="rule-item">No se generaron reglas específicas</div>'}
              </div>
            </div>
          </div>
        </div>
        
        <!-- Evaluación de Seguridad -->
        <div class="analysis-section">
          <h4><i class="fas fa-shield-alt"></i> Evaluación Autónoma de Seguridad</h4>
          <div class="safety-assessment ${analysis.safetyAssessment?.overallRisk?.toLowerCase() || 'bajo'}">
            <div class="safety-header">
              <div class="safety-icon">
                <i class="fas ${analysis.safetyAssessment?.safe ? 'fa-check-circle' : 'fa-exclamation-triangle'}"></i>
              </div>
              <div class="safety-info">
                <div class="safety-title">Riesgo Global: ${analysis.safetyAssessment?.overallRisk || 'BAJO'}</div>
                <div class="safety-details">
                  ${analysis.safetyAssessment?.highRiskObjects || 0} objetos de alto riesgo / 
                  ${analysis.safetyAssessment?.totalObjects || 0} total - 
                  Ratio: ${analysis.safetyAssessment?.riskRatio || '0'}
                </div>
              </div>
            </div>
            <div class="safety-recommendations">
              ${analysis.safetyAssessment?.recommendations?.map(rec => `
                <div class="safety-recommendation">${rec}</div>
              `).join('') || '<div class="safety-recommendation">Escena segura para observación</div>'}
            </div>
          </div>
        </div>
      </div>
      
      <!-- Sección Prolog Interactiva -->
      <div class="prolog-interactive-section">
        <h4><i class="fas fa-code"></i> Base de Conocimiento Prolog Generada</h4>
        
        <!-- Consultas Automáticas Generadas -->
        <div class="auto-queries-section">
          <h5>Consultas Automáticas Sugeridas:</h5>
          <div class="auto-queries-grid" id="dynamicQueriesGrid">
            <!-- Las consultas se generarán dinámicamente -->
          </div>
        </div>
        
        <!-- Editor de Consultas -->
        <div class="query-editor-section">
          <h5>Editor de Consultas Prolog:</h5>
          <div class="query-input-group">
            <textarea id="dynamicPrologQuery" class="form-control" placeholder="Escribe tu consulta Prolog aquí..." rows="3"></textarea>
            <button class="btn btn-primary" onclick="executeDynamicQuery()">
              <i class="fas fa-play"></i> Ejecutar Consulta
            </button>
          </div>
        </div>
        
        <!-- Resultados de Consultas -->
        <div class="query-results-section" id="dynamicQueryResults" style="display: none;">
          <h5>Resultados de la Consulta:</h5>
          <div class="result-content" id="dynamicQueryOutput"></div>
        </div>
        
        <!-- Base de Conocimiento -->
        <div class="knowledge-base-section">
          <h5>Base de Conocimiento Completa:</h5>
          <div class="code-container">
            <pre><code>${prologFacts}</code></pre>
          </div>
          <button class="btn btn-primary btn-sm" onclick="addToCustomRules(\`${prologFacts.replace(/`/g, '\\`')}\`)">
            <i class="fas fa-plus"></i> Cargar en Editor Prolog
          </button>
        </div>
      </div>
    </div>
  `;

  const uploadCard = document.querySelector('.card');
  uploadCard.parentNode.insertBefore(resultsContainer, uploadCard.nextSibling);

  // Generar consultas dinámicas basadas en el análisis
  generateDynamicQueries(analysis);
}

// Generar consultas Prolog dinámicas basadas en el análisis
function generateDynamicQueries(analysis) {
  const queriesGrid = document.getElementById('dynamicQueriesGrid');
  if (!queriesGrid) return;

  const dynamicQueries = [];

  // Consultas basadas en objetos detectados
  if (analysis.detectedObjects && analysis.detectedObjects.length > 0) {
    analysis.detectedObjects.forEach((obj, index) => {
      dynamicQueries.push({
        name: `Consultar ${obj.object}`,
        query: `objeto_detectado(${index + 1}, '${obj.object}', Confianza).`,
        description: `Información completa del ${obj.object}`
      });

      if (parseFloat(obj.confidence) > 80) {
        dynamicQueries.push({
          name: `${obj.object} - Alto riesgo`,
          query: `objeto_detectado(ID, '${obj.object}', Confianza), nivel_riesgo(ID, alto).`,
          description: `Verificar si ${obj.object} es de alto riesgo`
        });
      }
    });
  }

  // Consultas basadas en características
  if (analysis.characteristics) {
    Object.values(analysis.characteristics).forEach(char => {
      if (char.id) {
        dynamicQueries.push({
          name: `Características de ${char.nombre}`,
          query: `caracteristica_observable(${char.id}, Caracteristica).`,
          description: `Todas las características de ${char.nombre}`
        });

        dynamicQueries.push({
          name: `Clasificación ${char.nombre}`,
          query: `clasificacion_objeto(${char.id}, Clasificacion), nivel_riesgo(${char.id}, Riesgo).`,
          description: `Clasificación y riesgo de ${char.nombre}`
        });
      }
    });
  }

  // Consultas contextuales
  dynamicQueries.push(
    {
      name: "Resumen de Seguridad",
      query: "escena_segura(Segura), riesgo_global(Riesgo), objetos_alto_riesgo(AltoRiesgo).",
      description: "Estado general de seguridad de la escena"
    },
    {
      name: "Objetos Principales",
      query: "objeto_principal(ID), objeto_detectado(ID, Objeto, Confianza).",
      description: "Objetos más importantes detectados"
    },
    {
      name: "Análisis por Tipo",
      query: "tipo_escena(Tipo), ambiente(Ambiente).",
      description: "Información contextual de la escena"
    },
    {
      name: "Elementos Peligrosos",
      query: "elemento_peligroso(ID), objeto(ID, Nombre).",
      description: "Todos los elementos considerados peligrosos"
    }
  );

  // Mostrar las consultas
  let html = '';
  dynamicQueries.forEach((query, index) => {
    html += `
      <div class="auto-query-card" onclick="loadDynamicQuery('${query.query.replace(/'/g, "\\'")}', ${index})">
        <div class="auto-query-header">
          <i class="fas fa-play-circle"></i>
          <span class="auto-query-name">${query.name}</span>
        </div>
        <div class="auto-query-desc">${query.description}</div>
        <div class="auto-query-preview">${query.query}</div>
      </div>
    `;
  });

  queriesGrid.innerHTML = html;
}

// Cargar consulta dinámica en el editor
function loadDynamicQuery(query, index) {
  const queryEditor = document.getElementById('dynamicPrologQuery');
  if (queryEditor) {
    queryEditor.value = query;
    queryEditor.focus();

    // Efecto visual
    queryEditor.style.borderColor = '#007bff';
    queryEditor.style.boxShadow = '0 0 10px rgba(0, 123, 255, 0.3)';
    setTimeout(() => {
      queryEditor.style.borderColor = '';
      queryEditor.style.boxShadow = '';
    }, 2000);
  }
}

// Ejecutar consulta dinámica
async function executeDynamicQuery() {
  const queryEditor = document.getElementById('dynamicPrologQuery');
  const resultsDiv = document.getElementById('dynamicQueryResults');
  const outputDiv = document.getElementById('dynamicQueryOutput');

  if (!queryEditor || !queryEditor.value.trim()) {
    showAlert('⚠️ Por favor, ingresa una consulta Prolog', 'warning');
    return;
  }

  showLoading();

  try {
    const customRules = document.getElementById('customRules').value;
    const response = await fetch('/query/prolog', {
      method: 'POST',
      headers: {
        'Content-Type': 'application/json'
      },
      body: JSON.stringify({
        query: queryEditor.value.trim(),
        customRules,
        sessionId: appState.sessionId
      })
    });

    const result = await response.json();

    if (result.success) {
      outputDiv.textContent = JSON.stringify(result.results, null, 2);
      resultsDiv.style.display = 'block';
      showAlert(`✅ Consulta ejecutada: ${result.count} resultados`, 'success');
    } else {
      throw new Error(result.error);
    }
  } catch (error) {
    showAlert(`❌ Error en consulta Prolog: ${error.message}`, 'danger');
    outputDiv.textContent = `Error: ${error.message}`;
    resultsDiv.style.display = 'block';
  } finally {
    hideLoading();
  }
}

// Cargar sugerencias de consultas autónomas
async function loadAutonomousQuerySuggestions(analysis) {
  try {
    const response = await fetch(`/query/suggestions/${appState.sessionId}`);
    const result = await response.json();

    if (result.success) {
      displayAutonomousQueries(result.suggestions, analysis);
    }
  } catch (error) {
    console.log('No se pudieron cargar sugerencias:', error);
  }
}

// Mostrar consultas autónomas
function displayAutonomousQueries(suggestions, analysis) {
  const autoQueriesContainer = document.getElementById('autoQueriesContainer');
  if (!autoQueriesContainer) return;

  let html = `
        <div class="autonomous-queries-section">
            <h4><i class="fas fa-brain"></i> Consultas del Sistema Autónomo</h4>
            <div class="query-categories">
    `;

  // Consultas básicas
  html += `
        <div class="query-category">
            <h5>Consultas Básicas</h5>
            <div class="query-buttons">
    `;
  suggestions.slice(0, 3).forEach(query => {
    html += `
            <button class="btn btn-outline btn-sm" onclick="loadAutoQuery('${query.replace(/'/g, "\\'")}')">
                <i class="fas fa-play"></i> ${query.split(' %')[0]}
            </button>
        `;
  });
  html += `</div></div>`;

  // Consultas de seguridad
  html += `
        <div class="query-category">
            <h5>Análisis de Riesgo</h5>
            <div class="query-buttons">
                <button class="btn btn-outline btn-sm" onclick="loadAutoQuery('requiere_precaucion(X).')">
                    <i class="fas fa-shield-alt"></i> Objetos que requieren precaución
                </button>
                <button class="btn btn-outline btn-sm" onclick="loadAutoQuery('nivel_riesgo(X, alto).')">
                    <i class="fas fa-exclamation-triangle"></i> Objetos de alto riesgo
                </button>
                <button class="btn btn-outline btn-sm" onclick="loadAutoQuery('es_seguro(X).')">
                    <i class="fas fa-check-circle"></i> Objetos seguros
                </button>
            </div>
        </div>
    `;

  // Consultas de características
  html += `
        <div class="query-category">
            <h5>Análisis de Características</h5>
            <div class="query-buttons">
                <button class="btn btn-outline btn-sm" onclick="loadAutoQuery('caracteristica_observable(X, color_rojo_intenso).')">
                    <i class="fas fa-palette"></i> Objetos rojos intensos
                </button>
                <button class="btn btn-outline btn-sm" onclick="loadAutoQuery('tiene_caracteristica_peligrosa(X).')">
                    <i class="fas fa-skull-crossbones"></i> Características peligrosas
                </button>
                <button class="btn btn-outline btn-sm" onclick="loadAutoQuery('objetos_similares(X, Y).')">
                    <i class="fas fa-project-diagram"></i> Objetos similares
                </button>
            </div>
        </div>
    `;

  html += `</div></div>`;
  autoQueriesContainer.innerHTML = html;
  autoQueriesContainer.style.display = 'block';
}

// ... (el resto de las funciones se mantienen igual)

function loadAutoQuery(query) {
  prologQuery.value = query;
  prologQuery.focus();

  // Resaltar visualmente
  prologQuery.style.borderColor = '#007bff';
  prologQuery.style.boxShadow = '0 0 10px rgba(0, 123, 255, 0.3)';
  setTimeout(() => {
    prologQuery.style.borderColor = '';
    prologQuery.style.boxShadow = '';
  }, 2000);
}

// Mostrar información del archivo
function displayFileInfo(file, stats) {
  const fileSize = (file.size / 1024 / 1024).toFixed(2);
  fileInfo.innerHTML = `
        <p><strong>Nombre:</strong> ${file.name}</p>
        <p><strong>Tamaño:</strong> ${fileSize} MB</p>
        <p><strong>Tipo:</strong> ${file.type || 'Desconocido'}</p>
        <p><strong>Registros:</strong> ${stats.totalRecords}</p>
        <p><strong>Columnas:</strong> ${stats.columns}</p>
    `;
  fileInfo.style.display = 'block';
}

// Mostrar tabla de datos
function displayDataTable(data) {
  if (!data || data.length === 0) {
    dataTable.innerHTML = '<p class="text-muted">No hay datos para mostrar</p>';
    return;
  }

  const headers = Object.keys(data[0]);
  let html = `
        <table>
            <thead>
                <tr>
                    ${headers.map(header => `<th>${header}</th>`).join('')}
                </tr>
            </thead>
            <tbody>
    `;

  data.forEach(row => {
    html += '<tr>';
    headers.forEach(header => {
      const value = row[header];
      html += `<td>${value !== null && value !== undefined ? value : ''}</td>`;
    });
    html += '</tr>';
  });

  html += `
            </tbody>
        </table>
    `;

  dataTable.innerHTML = html;
}

// Mostrar estadísticas
function displayStats(stats) {
  if (!stats) return;

  let html = '';

  // Estadísticas generales
  html += `
        <div class="stat-card">
            <div class="stat-icon">
                <i class="fas fa-database"></i>
            </div>
            <div class="stat-info">
                <div class="stat-value">${stats.totalRecords}</div>
                <div class="stat-label">Registros Totales</div>
            </div>
        </div>
        <div class="stat-card">
            <div class="stat-icon">
                <i class="fas fa-columns"></i>
            </div>
            <div class="stat-info">
                <div class="stat-value">${stats.columns}</div>
                <div class="stat-label">Columnas</div>
            </div>
        </div>
    `;

  // Estadísticas por columna
  Object.entries(stats.columnStats).forEach(([column, columnStats]) => {
    html += `
            <div class="stat-card">
                <div class="stat-icon">
                    <i class="fas fa-chart-bar"></i>
                </div>
                <div class="stat-info">
                    <div class="stat-value">${column}</div>
                    <div class="stat-label">
                        ${columnStats.type} | ${columnStats.nonNull} no nulos
                        ${columnStats.unique ? `| ${columnStats.unique} únicos` : ''}
                    </div>
                </div>
            </div>
        `;
  });

  statsGrid.innerHTML = html;
  statsCard.style.display = 'block';
}

// 🔥 NUEVA FUNCIÓN: Usar reglas específicas en consultas
async function useSavedRuleInQueries(ruleName) {
  try {
    const response = await fetch(`/rules/load/${appState.sessionId}/${ruleName}`);
    const result = await response.json();

    if (result.success) {
      // Mostrar las reglas en el área de consultas
      const prologQuery = document.getElementById('prologQuery');
      const currentQuery = prologQuery.value.trim();

      if (currentQuery) {
        // Si ya hay una consulta, agregar las reglas como comentario para referencia
        prologQuery.value = `% Reglas de: ${ruleName}\n% ${result.rules.replace(/\n/g, '\n% ')}\n\n${currentQuery}`;
      } else {
        // Si no hay consulta, mostrar ejemplos de uso
        prologQuery.value = `% Reglas cargadas: ${ruleName}\n% ${result.rules.replace(/\n/g, '\n% ')}\n\n% Ejemplos de uso:\n% objeto(X, Y), clasificacion_objeto(X, Z).\n% nivel_riesgo(X, alto), caracteristica_observable(X, C).`;
      }

      showAlert(`✅ Reglas "${ruleName}" cargadas para usar en consultas`, 'success');
    }
  } catch (error) {
    showAlert(`❌ Error cargando reglas: ${error.message}`, 'danger');
  }
}

// 🔥 FUNCIÓN CORREGIDA: Ejecutar consultas Prolog
async function executePrologQuery() {
    const queryInput = document.getElementById('prologQuery');
    const query = queryInput.value.trim();

    if (!query) {
        showNotification('warning', 'Consulta vacía', 'Por favor ingresa una consulta Prolog');
        return;
    }

    // Validar que sea una consulta, no una regla
    const validation = validatePrologQuery(query);
    if (!validation.isValid) {
        showNotification('error', 'Error de sintaxis', validation.error);
        return;
    }

    showLoading('querying', 'Ejecutando consulta...');

    try {
        console.log(`🚀 Ejecutando consulta: ${query}`);
        
        const result = await executeSingleQueryHybrid(query);
        
        // Mostrar resultados
        displayPrologResults(result.results, result.count);
        
        if (result.success) {
            if (result.count > 0) {
                showNotification('success', 'Consulta exitosa', 
                    `Se encontraron ${result.count} resultados`);
            } else {
                showNotification('info', 'Sin resultados', 
                    'La consulta se ejecutó pero no devolvió resultados');
            }
        } else {
            showNotification('warning', 'Consulta con advertencias', 
                result.error || 'La consulta se ejecutó con advertencias');
        }

    } catch (error) {
        console.error('❌ Error ejecutando consulta:', error);
        showNotification('error', 'Error en consulta', 
            `No se pudo ejecutar la consulta: ${error.message}`);
    } finally {
        hideLoading();
    }
}

// 🔧 FUNCIÓN DE DIAGNÓSTICO MEJORADA
async function testPrologConnection() {
  const testQueries = [
    'objeto_detectado(X, Y, Z).',
    'member(a, [a,b,c]).',
    'length([1,2,3], L).',
    'nivel_riesgo(X, bajo).'
  ];

  showLoading();
  let allTestsPassed = true;
  const results = [];

  for (let i = 0; i < testQueries.length; i++) {
    const query = testQueries[i];
    try {
      console.log(`🧪 Probando consulta: ${query}`);
      const response = await fetch('/query/prolog/simple', {
        method: 'POST',
        headers: {
          'Content-Type': 'application/json'
        },
        body: JSON.stringify({
          query,
          sessionId: appState.sessionId
        })
      });

      const result = await response.json();
      const testPassed = result.success && result.count >= 0;

      console.log(`Test ${i + 1}:`, testPassed ? '✅ ÉXITO' : '❌ FALLO', result);
      results.push({ query, success: testPassed, result });

      if (!testPassed) {
        allTestsPassed = false;
      }

      await new Promise(resolve => setTimeout(resolve, 200));

    } catch (error) {
      console.error(`❌ Test ${i + 1} falló:`, error);
      results.push({ query, success: false, error: error.message });
      allTestsPassed = false;
    }
  }

  hideLoading();

  // Mostrar resultados detallados
  if (allTestsPassed) {
    showAlert('✅ Diagnóstico: Todas las pruebas pasaron correctamente', 'success');
  } else {
    let errorMessage = '❌ Diagnóstico: Algunas pruebas fallaron\n\n';
    results.forEach((test, index) => {
      errorMessage += `Test ${index + 1}: ${test.query} - ${test.success ? '✅' : '❌'}\n`;
      if (!test.success) {
        errorMessage += `   Error: ${test.error || test.result?.error}\n`;
      } else {
        errorMessage += `   Resultados: ${test.result?.count}\n`;
      }
    });

    prologOutput.textContent = errorMessage;
    prologResults.style.display = 'block';
    showAlert('❌ Diagnóstico: Ver resultados para detalles', 'warning');
  }

  return allTestsPassed;
}

// Agregar consultas rápidas específicas para las reglas guardadas
function setupQuickQueries() {
  const quickQueries = [
    {
      name: "Objetos detectados",
      query: "objeto_detectado(Id, Objeto, Confianza).",
      icon: "fa-search"
    },
    {
      name: "Objetos seguros",
      query: "nivel_riesgo(X, bajo).",
      icon: "fa-shield-alt"
    },
    {
      name: "Características",
      query: "caracteristica_observable(X, Caracteristica).",
      icon: "fa-list"
    },
    {
      name: "Consulta simple",
      query: "member(X, [1,2,3]).",
      icon: "fa-vial"
    }
  ];

  const quickQueriesContainer = document.getElementById('quickQueries');
  if (!quickQueriesContainer) return;

  let html = '<div class="quick-queries"><h4>Consultas Rápidas</h4><div class="quick-query-buttons">';

  quickQueries.forEach(q => {
    html += `
            <button class="btn btn-outline btn-sm quick-query-btn" onclick="loadAutoQuery('${q.query}')">
                <i class="fas ${q.icon}"></i> ${q.name}
            </button>
        `;
  });

  html += '</div></div>';
  quickQueriesContainer.innerHTML = html;
}

// Agregar botón de diagnóstico en la inicialización
function addDiagnosticButton() {
  const querySection = document.querySelector('.query-suggestions');
  if (querySection) {
    const diagnosticButton = document.createElement('button');
    diagnosticButton.className = 'btn btn-outline btn-sm';
    diagnosticButton.innerHTML = '<i class="fas fa-stethoscope"></i> Diagnóstico Prolog';
    diagnosticButton.onclick = testPrologConnection;
    diagnosticButton.style.marginLeft = '10px';

    const suggestions = querySection.querySelector('.suggestion-buttons');
    if (suggestions) {
      suggestions.appendChild(diagnosticButton);
    }
  }
}

// 🔥 FUNCIÓN MEJORADA: Mostrar resultados Prolog
function displayPrologResults(results, count) {
    const resultCount = document.getElementById('resultCount');
    const prologOutput = document.getElementById('prologOutput');
    const prologResults = document.getElementById('prologResults');

    if (!resultCount || !prologOutput || !prologResults) {
        console.error('❌ Elementos de resultados no encontrados');
        return;
    }

    // Actualizar contador
    resultCount.textContent = `${count} ${count === 1 ? 'resultado' : 'resultados'}`;

    // Mostrar resultados
    if (!results || results.length === 0) {
        prologOutput.innerHTML = `
            <div class="no-results">
                <i class="fas fa-search"></i>
                <h4>No se encontraron resultados</h4>
                <p>La consulta se ejecutó correctamente pero no devolvió soluciones.</p>
                <div class="suggestion">
                    <strong>Sugerencias:</strong>
                    <ul>
                        <li>Verifica que los datos estén cargados</li>
                        <li>Prueba con consultas más simples</li>
                        <li>Asegúrate de que las reglas estén definidas</li>
                    </ul>
                </div>
            </div>
        `;
    } else {
        let html = '<div class="results-grid">';
        
        results.forEach((result, index) => {
            html += `
                <div class="result-item">
                    <div class="result-header">
                        <span class="result-number">#${index + 1}</span>
                        <span class="result-type">Solución</span>
                    </div>
                    <div class="result-content">
            `;
            
            // Mostrar cada variable del resultado
            Object.entries(result).forEach(([variable, value]) => {
                if (variable !== 'undefined' && value !== 'null') {
                    html += `
                        <div class="variable-binding">
                            <span class="variable-name">${variable}</span>
                            <span class="binding-operator"> = </span>
                            <span class="variable-value">${value}</span>
                        </div>
                    `;
                }
            });
            
            html += `
                    </div>
                </div>
            `;
        });
        
        html += '</div>';
        prologOutput.innerHTML = html;
    }

    // Mostrar contenedor de resultados
    prologResults.style.display = 'block';
}

// Mostrar análisis de imagen
function displayImageAnalysis(analysis, prologFacts) {
  // Eliminar análisis previo si existe
  const prevAnalysis = document.querySelector('.image-analysis-results');
  if (prevAnalysis) {
    prevAnalysis.remove();
  }

  const resultsContainer = document.createElement('div');
  resultsContainer.className = 'image-analysis-results';
  resultsContainer.innerHTML = `
        <div class="card">
            <div class="card-header">
                <div class="card-title">
                    <i class="fas fa-robot"></i>
                    Análisis de Imagen por IA
                </div>
            </div>
            
            <div class="analysis-grid">
                <div class="analysis-section">
                    <h4><i class="fas fa-info-circle"></i> Información Básica</h4>
                    <div class="info-grid">
                        <div class="info-item">
                            <span class="info-label">Resolución:</span>
                            <span class="info-value">${analysis.width} × ${analysis.height}</span>
                        </div>
                        <div class="info-item">
                            <span class="info-label">Formato:</span>
                            <span class="info-value">${analysis.format}</span>
                        </div>
                        <div class="info-item">
                            <span class="info-label">Tamaño:</span>
                            <span class="info-value">${analysis.features.estimatedSize}</span>
                        </div>
                        <div class="info-item">
                            <span class="info-label">Brillo:</span>
                            <span class="info-value">${analysis.features.brightness}</span>
                        </div>
                        <div class="info-item">
                            <span class="info-label">Relación Aspecto:</span>
                            <span class="info-value">${analysis.features.aspectRatio}</span>
                        </div>
                    </div>
                </div>
                
                <div class="analysis-section">
                    <h4><i class="fas fa-object-group"></i> Objetos Detectados</h4>
                    <div class="objects-list">
                        ${analysis.detectedObjects.map(obj => `
                            <div class="object-item">
                                <span class="object-name">${obj.object}</span>
                                <span class="object-confidence ${getConfidenceClass(obj.confidence)}">
                                    ${obj.confidence}
                                </span>
                            </div>
                        `).join('')}
                    </div>
                </div>
                
                <div class="analysis-section">
                    <h4><i class="fas fa-palette"></i> Colores Dominantes</h4>
                    <div class="colors-grid">
                        ${analysis.colors.map(color => `
                            <div class="color-item">
                                <div class="color-swatch" style="background-color: ${color.color}"></div>
                                <span class="color-percentage">${color.percentage}</span>
                            </div>
                        `).join('')}
                    </div>
                </div>
            </div>
            
            <div class="prolog-section">
                <h4><i class="fas fa-code"></i> Hechos Prolog Generados</h4>
                <div class="code-container">
                    <code>${prologFacts}</code>
                </div>
                <button class="btn btn-primary btn-sm" onclick="addToCustomRules(\`${prologFacts.replace(/`/g, '\\`')}\`)">
                    <i class="fas fa-plus"></i> Agregar a Reglas Personalizadas
                </button>
            </div>
        </div>
    `;

  // Insertar después de la sección de carga
  const uploadCard = document.querySelector('.card');
  uploadCard.parentNode.insertBefore(resultsContainer, uploadCard.nextSibling);
}

function getConfidenceClass(confidence) {
  const confValue = parseFloat(confidence);
  if (confValue >= 80) return 'high-confidence';
  if (confValue >= 60) return 'medium-confidence';
  return 'low-confidence';
}

// 🔥 NUEVA FUNCIÓN: Dividir reglas en cards individuales
function splitRulesIntoIndividualCards(rulesText) {
  if (!rulesText || !rulesText.trim()) return [];

  const lines = rulesText.split('\n')
    .map(line => line.trim())
    .filter(line => {
      // Filtrar solo líneas que son reglas Prolog válidas
      return line &&
        !line.startsWith('%') &&
        line.endsWith('.') &&
        line.length > 3;
    });

  const individualRules = [];

  lines.forEach((line, index) => {
    // Extraer el nombre de la regla (parte antes del :- o del .)
    let ruleName = 'regla';
    if (line.includes(':-')) {
      ruleName = line.split(':-')[0].trim();
    } else {
      ruleName = line.substring(0, line.length - 1).trim();
    }

    // Limpiar el nombre para que sea legible
    ruleName = ruleName.replace(/\(.*\)/, '') // Remover parámetros
      .replace(/,/g, '_')
      .replace(/'/g, '')
      .substring(0, 30); // Limitar longitud

    individualRules.push({
      id: Date.now() + index, // ID único
      name: ruleName || `Regla ${index + 1}`,
      code: line,
      timestamp: new Date().toISOString(),
      type: 'individual'
    });
  });

  return individualRules;
}
// 🔥 ACTUALIZAR: Función saveRules para crear cards individuales
async function saveRules() {
  const rules = customRules.value.trim();
  const ruleName = document.getElementById('ruleName').value || `conjunto_reglas_${Date.now()}`;

  if (!rules) {
    showNotification('warning', 'Advertencia', 'No hay reglas para guardar');
    return;
  }

  try {
    // Guardar en el servidor (mantener funcionalidad existente)
    const response = await fetch('/rules/save', {
      method: 'POST',
      headers: {
        'Content-Type': 'application/json'
      },
      body: JSON.stringify({
        rules,
        ruleName,
        sessionId: appState.sessionId
      })
    });

    const result = await response.json();

    if (result.success) {
      // 🔥 DIVIDIR REGLAS EN CARDS INDIVIDUALES
      const individualRules = splitRulesIntoIndividualCards(rules);

      if (individualRules.length === 0) {
        showNotification('warning', 'Sin reglas', 'No se encontraron reglas individuales para crear cards');
        return;
      }

      // Agregar cada regla individual al carrusel
      individualRules.forEach(rule => {
        carouselState.savedRules.unshift(rule);
      });

      carouselState.currentRuleSet = carouselState.savedRules;

      // Guardar en localStorage
      localStorage.setItem('ruleCards', JSON.stringify(carouselState.savedRules));

      // Actualizar carrusel
      updateCarousel();
      updateCarouselInfo();

      showNotification('success', 'Cards Creadas',
        `Se crearon ${individualRules.length} cards individuales a partir de las reglas`);

    } else {
      throw new Error(result.error);
    }
  } catch (error) {
    showNotification('error', 'Error',
      `No se pudieron guardar las reglas: ${error.message}`);
  }
}


// 🔥 NUEVAS FUNCIONES: Gestión de reglas individuales
function editRule(ruleIndex) {
  const rule = carouselState.currentRuleSet[ruleIndex];
  if (rule && rule.code) {
    customRules.value = rule.code;
    document.getElementById('ruleName').value = rule.name;

    showNotification('info', 'Regla Cargada',
      `"${rule.name}" ha sido cargada en el editor para modificaciones`);
  }
}

function deleteRule(ruleIndex) {
  const rule = carouselState.currentRuleSet[ruleIndex];
  if (!rule) return;

  if (confirm(`¿Estás seguro de que quieres eliminar la regla "${rule.name}"?`)) {
    // Eliminar del array
    carouselState.savedRules = carouselState.savedRules.filter(r => r.id !== rule.id);
    carouselState.currentRuleSet = carouselState.savedRules;

    // Guardar cambios
    localStorage.setItem('ruleCards', JSON.stringify(carouselState.savedRules));

    // Actualizar carrusel
    updateCarousel();
    updateCarouselInfo();

    showNotification('info', 'Regla Eliminada',
      `"${rule.name}" ha sido eliminada del carrusel`);
  }
}

function clearAllRules() {
  if (carouselState.savedRules.length === 0) {
    showNotification('info', 'Carrusel Vacío', 'No hay reglas para eliminar');
    return;
  }

  if (confirm(`¿Estás seguro de que quieres eliminar TODAS las reglas (${carouselState.savedRules.length})?`)) {
    carouselState.savedRules = [];
    carouselState.currentRuleSet = [];
    localStorage.removeItem('ruleCards');

    updateCarousel();
    updateCarouselInfo();

    showNotification('info', 'Carrusel Limpiado',
      'Todas las reglas han sido eliminadas');
  }
}

// 🔥 ACTUALIZAR: Función generateRules para agregar al carrusel
async function generateRules() {
  try {
    const response = await fetch('/rules/generate', {
      method: 'POST',
      headers: {
        'Content-Type': 'application/json'
      },
      body: JSON.stringify({
        sessionId: appState.sessionId,
        criteria: ['classification', 'safety', 'context']
      })
    });

    const result = await response.json();

    if (result.success) {
      addToCustomRules(result.rules);

      // 🔥 AGREGAR REGLAS GENERADAS AL CARRUSEL
      const generatedRule = {
        id: Date.now(),
        name: 'reglas_generadas_automaticamente',
        code: result.rules,
        timestamp: new Date().toISOString(),
        type: 'automática'
      };

      carouselState.savedRules.unshift(generatedRule);
      carouselState.currentRuleSet = carouselState.savedRules;
      localStorage.setItem('ruleCards', JSON.stringify(carouselState.savedRules));

      updateCarousel();
      updateCarouselInfo();

      showNotification('success', 'Reglas Generadas',
        'Se han generado reglas automáticamente y se han agregado al carrusel');

    } else {
      throw new Error(result.error);
    }
  } catch (error) {
    showNotification('error', 'Error',
      `No se pudieron generar las reglas: ${error.message}`);
  }
}

function addToCustomRules(newRules) {
  const currentRules = customRules.value;
  if (currentRules && !currentRules.endsWith('\n')) {
    customRules.value += '\n';
  }
  customRules.value += newRules + '\n';
}

// Plantillas de reglas
function loadTemplate(templateType) {
  const templates = {
    filter: `% Filtros básicos
es_grande(X) :- dato(X, Tamano), Tamano > 100.
es_pequeno(X) :- dato(X, Tamano), Tamano < 10.
es_rojo(X) :- dato(X, Color), Color = 'rojo'.`,

    classification: `% Clasificación
clasificar_importancia(X, alta) :- dato(X, Valor), Valor > 80.
clasificar_importancia(X, media) :- dato(X, Valor), Valor >= 50, Valor =< 80.
clasificar_importancia(X, baja) :- dato(X, Valor), Valor < 50.`,

    calculation: `% Cálculos
sumar(X, Y, Resultado) :- Resultado is X + Y.
promedio(Lista, Promedio) :- 
    sum_list(Lista, Suma),
    length(Lista, Length),
    Promedio is Suma / Length.`,

    validation: `% Validación
es_valido(X) :- dato(X, Valor), Valor >= 0, Valor =< 100.
tiene_datos(X) :- dato(X, _), \\+ dato(X, null).
cumple_condiciones(X) :- es_valido(X), tiene_datos(X).`
  };

  addToCustomRules(templates[templateType] || templates.filter);
}

// Entrenamiento de modelo
async function trainCustomModel() {
  showLoading();

  try {
    const response = await fetch('/train/model', {
      method: 'POST',
      headers: {
        'Content-Type': 'application/json'
      },
      body: JSON.stringify({
        modelType: 'custom_classifier',
        labels: ['mi_clase_1', 'mi_clase_2', 'mi_clase_3']
      })
    });

    const result = await response.json();

    if (result.success) {
      showAlert(`🎯 ${result.message}`, 'success');
      console.log('Info del modelo:', result.modelInfo);
    } else {
      throw new Error(result.error);
    }
  } catch (error) {
    showAlert(`❌ Error en entrenamiento: ${error.message}`, 'danger');
  } finally {
    hideLoading();
  }
}

// Utilidades de UI
function showAlert(message, type) {
  alertDiv.textContent = message;
  alertDiv.className = `alert alert-${type}`;
  alertDiv.style.display = 'block';

  setTimeout(() => {
    alertDiv.style.display = 'none';
  }, 5000);
}

// 🔥 FUNCIÓN MEJORADA: Mostrar loading
function showLoading(type = 'default', message = 'Procesando...', details = '') {
  if (!loadingDiv) {
    console.warn('Loading div no disponible');
    return;
  }

  // Actualizar estado
  loadingState.isShowing = true;
  loadingState.currentType = type;
  loadingState.progress = 0;

  // Determinar ícono y clase según el tipo
  let icon = '⏳';
  let loadingClass = 'loading';

  switch (type) {
    case 'analyzing':
      icon = '🔍';
      loadingClass += ' analyzing';
      message = message || 'Analizando...';
      break;
    case 'querying':
      icon = '⚡';
      loadingClass += ' querying';
      message = message || 'Ejecutando consulta...';
      break;
    case 'processing':
      icon = '🔄';
      loadingClass += ' processing';
      message = message || 'Procesando...';
      break;
    case 'saving':
      icon = '💾';
      message = message || 'Guardando...';
      break;
    case 'generating':
      icon = '✨';
      message = message || 'Generando...';
      break;
    default:
      icon = '⏳';
      message = message || 'Procesando...';
  }

  // Construir el contenido del loading
  let loadingHTML = `
        <div class="loading-particles">
            <div class="loading-particle"></div>
            <div class="loading-particle"></div>
            <div class="loading-particle"></div>
        </div>
        <div class="loading-content">
    `;

  if (type === 'progress') {
    loadingHTML += `
            <div class="loading-spinner"></div>
            <div>
                <p>${message}</p>
                <div class="progress-bar">
                    <div class="progress-fill" id="progressFill" style="width: 0%"></div>
                </div>
                <div class="progress-text" id="progressText">0%</div>
                ${details ? `<div class="loading-details">${details}</div>` : ''}
            </div>
        `;
  } else {
    loadingHTML += `
            ${icon ? `<div class="loading-icon">${icon}</div>` : ''}
            <div>
                <p>${message}</p>
                ${details ? `<div class="loading-status">${details}</div>` : ''}
                <div class="loading-spinner"></div>
            </div>
        `;
  }

  loadingHTML += `</div>`;

  // Aplicar al DOM
  loadingDiv.className = loadingClass;
  loadingDiv.innerHTML = loadingHTML;
  loadingDiv.style.display = 'block';

  // Efecto de entrada
  loadingDiv.style.animation = 'slideInRight 0.5s ease-out';
}

// 🔥 FUNCIÓN MEJORADA: Ocultar loading
function hideLoading() {
  if (!loadingDiv || !loadingState.isShowing) return;

  // Efecto de salida
  loadingDiv.style.animation = 'slideOutRight 0.5s ease-in forwards';

  // Ocultar después de la animación
  setTimeout(() => {
    if (loadingDiv) {
      loadingDiv.style.display = 'none';
      loadingState.isShowing = false;
    }
  }, 500);
}


function clearResults() {
  prologOutput.textContent = '';
  prologResults.style.display = 'none';
}

function exportResults() {
  const results = prologOutput.textContent;
  if (!results) {
    showAlert('⚠️ No hay resultados para exportar', 'warning');
    return;
  }

  const blob = new Blob([results], { type: 'text/plain' });
  const url = URL.createObjectURL(blob);
  const a = document.createElement('a');
  a.href = url;
  a.download = `resultados_prolog_${new Date().getTime()}.txt`;
  a.click();
  URL.revokeObjectURL(url);
}

function switchTab(tabName) {
  // Ocultar todos los tabs
  document.querySelectorAll('.tab-content').forEach(tab => {
    tab.classList.remove('active');
  });
  document.querySelectorAll('.tab-btn').forEach(btn => {
    btn.classList.remove('active');
  });

  // Mostrar tab seleccionado
  document.getElementById(tabName + 'Tab').classList.add('active');
  event.currentTarget.classList.add('active');
}

function updateSessionInfo() {
  const sessionElement = document.getElementById('sessionStatus');
  if (sessionElement) {
    sessionElement.textContent = `Sesión: ${appState.sessionId}`;
  }
}

function loadExamplePrologQueries() {
  const examples = [
    "dato(X, Y, Z), Y > 100.",
    "findall(X, dato(X, _, _), Resultados).",
    "objeto_detectado(_, Objeto, Confianza), Confianza > 80.",
    "color_dominante(_, Color, Porcentaje), Porcentaje > 20."
  ];

  prologQuery.placeholder = `Ejemplos:\n${examples.join('\n')}`;
}

function generatePrologExamples(headers, data) {
  if (!headers || !data.length) return;

  const suggestionButtons = document.getElementById('suggestionButtons');
  if (!suggestionButtons) return;

  let html = '';

  // Ejemplos basados en los datos
  headers.forEach(header => {
    const firstValue = data[0][header];
    if (typeof firstValue === 'number') {
      html += `<button class="btn btn-outline btn-sm" onclick="setPrologQuery('dato(X, ${header}, Valor), Valor > ${firstValue}.')">
                ${header} > ${firstValue}
            </button>`;
    } else {
      html += `<button class="btn btn-outline btn-sm" onclick="setPrologQuery('dato(X, ${header}, '${firstValue}').')">
                ${header} = '${firstValue}'
            </button>`;
    }
  });

  suggestionButtons.innerHTML = html;
}

function setPrologQuery(query) {
  prologQuery.value = query;
}

// Análisis de datos
async function performAnalysis() {
  const analysisType = document.getElementById('analysisType').value;
  const params = document.getElementById('analysisParams').value;

  showLoading();

  try {
    const response = await fetch('/analyze/data', {
      method: 'POST',
      headers: {
        'Content-Type': 'application/json'
      },
      body: JSON.stringify({
        analysisType,
        parameters: params,
        sessionId: appState.sessionId
      })
    });

    const result = await response.json();

    if (result.success) {
      displayAnalysisResults(result.analysis);
      showAlert(`📊 ${result.message}`, 'success');
    } else {
      throw new Error(result.error);
    }
  } catch (error) {
    showAlert(`❌ Error en análisis: ${error.message}`, 'danger');
  } finally {
    hideLoading();
  }
}

function displayAnalysisResults(analysis) {
  const analysisResults = document.getElementById('analysisResults');
  const analysisOutput = document.getElementById('analysisOutput');

  analysisOutput.textContent = JSON.stringify(analysis, null, 2);
  analysisResults.style.display = 'block';
}

// Análisis avanzado
async function performAdvancedAnalysis(file, analysisType = 'comprehensive') {
  showLoading();

  const formData = new FormData();
  formData.append('file', file);
  formData.append('sessionId', appState.sessionId);
  formData.append('analysisType', analysisType);

  try {
    const response = await fetch('/analyze/advanced', {
      method: 'POST',
      body: formData
    });

    const result = await response.json();

    if (result.success) {
      displayAdvancedAnalysis(result.analysis);
      showAlert(`🔍 ${result.message}`, 'success');
    } else {
      throw new Error(result.error);
    }
  } catch (error) {
    showAlert(`❌ Error en análisis avanzado: ${error.message}`, 'danger');
  } finally {
    hideLoading();
  }
}

function displayAdvancedAnalysis(analysis) {
  const resultsContainer = document.createElement('div');
  resultsContainer.className = 'advanced-analysis-results';

  let html = `
        <div class="card">
            <div class="card-header">
                <div class="card-title">
                    <i class="fas fa-chart-line"></i>
                    Análisis Avanzado
                </div>
                <div class="card-actions">
                    <button class="btn btn-outline btn-sm" onclick="generateReport()">
                        <i class="fas fa-file-pdf"></i> Generar Reporte
                    </button>
                </div>
            </div>
            
            <div class="analysis-sections">
    `;

  // Resumen
  if (analysis.summary) {
    html += `
            <div class="analysis-section">
                <h4><i class="fas fa-info-circle"></i> Resumen</h4>
                <div class="stats-grid">
                    <div class="stat-card">
                        <div class="stat-value">${analysis.summary.totalRecords || 0}</div>
                        <div class="stat-label">Registros</div>
                    </div>
                    <div class="stat-card">
                        <div class="stat-value">${analysis.summary.totalColumns || 0}</div>
                        <div class="stat-label">Columnas</div>
                    </div>
                </div>
            </div>
        `;
  }

  // Insights
  if (analysis.insights && analysis.insights.length > 0) {
    html += `
            <div class="analysis-section">
                <h4><i class="fas fa-lightbulb"></i> Insights</h4>
                <div class="insights-list">
                    ${analysis.insights.map(insight => `
                        <div class="insight-item">
                            <div class="insight-header">
                                <span class="insight-type">${insight.type}</span>
                                <span class="insight-column">${insight.column}</span>
                            </div>
                            <div class="insight-details">
                                ${Object.entries(insight).filter(([key]) => !['type', 'column'].includes(key))
        .map(([key, value]) => `<span>${key}: ${JSON.stringify(value)}</span>`)
        .join(' | ')}
                            </div>
                        </div>
                    `).join('')}
                </div>
            </div>
        `;
  }

  // Patrones
  if (analysis.patterns && analysis.patterns.length > 0) {
    html += `
            <div class="analysis-section">
                <h4><i class="fas fa-project-diagram"></i> Patrones Detectados</h4>
                <div class="patterns-list">
                    ${analysis.patterns.map(pattern => `
                        <div class="pattern-item ${pattern.type}">
                            <i class="fas fa-${getPatternIcon(pattern.type)}"></i>
                            <div class="pattern-content">
                                <div class="pattern-message">${pattern.message || pattern.type}</div>
                                ${pattern.columns ? `<div class="pattern-columns">Columnas: ${pattern.columns.join(', ')}</div>` : ''}
                            </div>
                        </div>
                    `).join('')}
                </div>
            </div>
        `;
  }

  // Recomendaciones
  if (analysis.recommendations && analysis.recommendations.length > 0) {
    html += `
            <div class="analysis-section">
                <h4><i class="fas fa-bullhorn"></i> Recomendaciones</h4>
                <div class="recommendations-list">
                    ${analysis.recommendations.map(rec => `
                        <div class="recommendation-item ${rec.type}">
                            <i class="fas fa-${getRecommendationIcon(rec.type)}"></i>
                            <span>${rec.message}</span>
                        </div>
                    `).join('')}
                </div>
            </div>
        `;
  }

  html += `
            </div>
        </div>
    `;

  resultsContainer.innerHTML = html;

  // Insertar después de la sección de estadísticas
  const statsCard = document.getElementById('statsCard');
  if (statsCard) {
    statsCard.parentNode.insertBefore(resultsContainer, statsCard.nextSibling);
  } else {
    document.querySelector('.container').appendChild(resultsContainer);
  }
}

function getPatternIcon(patternType) {
  const icons = {
    'outliers_detected': 'exclamation-triangle',
    'correlation_suggestion': 'project-diagram',
    'trend_detected': 'chart-line'
  };
  return icons[patternType] || 'chart-bar';
}

function getRecommendationIcon(recommendationType) {
  const icons = {
    'warning': 'exclamation-triangle',
    'data_quality': 'database',
    'optimization': 'rocket'
  };
  return icons[recommendationType] || 'info-circle';
}

// Generar reporte
async function generateReport() {
  try {
    const response = await fetch(`/report/${appState.sessionId}`);
    const result = await response.json();

    if (result.success) {
      // Descargar reporte como JSON
      const blob = new Blob([JSON.stringify(result.report, null, 2)], {
        type: 'application/json'
      });
      const url = URL.createObjectURL(blob);
      const a = document.createElement('a');
      a.href = url;
      a.download = `reporte_analisis_${new Date().getTime()}.json`;
      a.click();
      URL.revokeObjectURL(url);

      showAlert('📄 Reporte generado y descargado', 'success');
    } else {
      throw new Error(result.error);
    }
  } catch (error) {
    showAlert(`❌ Error generando reporte: ${error.message}`, 'danger');
  }
}

// Cargar plantillas de reglas
async function loadRuleTemplates() {
  try {
    const response = await fetch('/rules/templates');
    const result = await response.json();

    if (result.success) {
      // Agregar botones de plantillas
      const templatesContainer = document.createElement('div');
      templatesContainer.className = 'templates-section';
      templatesContainer.innerHTML = `
                <h4><i class="fas fa-magic"></i> Plantillas de Reglas</h4>
                <div class="template-buttons">
                    ${Object.entries(result.templates).map(([name, template]) => `
                        <button class="btn btn-outline btn-sm" onclick="loadRuleTemplate('${name}')">
                            <i class="fas fa-cube"></i> ${name.replace('_', ' ')}
                        </button>
                    `).join('')}
                </div>
            `;

      const rulesCard = document.querySelector('.card .prolog-examples');
      if (rulesCard) {
        rulesCard.parentNode.insertBefore(templatesContainer, rulesCard);
      }
    }
  } catch (error) {
    console.log('No se pudieron cargar las plantillas:', error.message);
  }
}

function loadRuleTemplate(templateName) {
  // Esto cargaría la plantilla cuando se implemente la ruta
  showAlert(`📝 Plantilla "${templateName}" seleccionada - Implementar carga`, 'info');
}

// // Mejorar el manejo de archivos para incluir análisis avanzado
// function handleFileSelect(e, type) {
//   if (e.target.files.length > 0) {
//     const file = e.target.files[0];
//     if (type === 'data') {
//       // Opción: procesamiento normal o avanzado
//       if (confirm('¿Desea realizar un análisis avanzado del archivo?')) {
//         performAdvancedAnalysis(file);
//       } else {
//         processDataFile(file);
//       }
//     } else {
//       processImageFile(file);
//     }
//   }
// }

// Generar consultas automáticas basadas en datos
function generateAutomaticQueries(data, headers, analysisType = 'data') {
  const queries = [];

  if (analysisType === 'data' && data.length > 0) {
    // Consultas básicas para datos
    queries.push({
      name: "Todos los registros",
      query: "dato(Id, _, _).",
      description: "Obtener todos los registros"
    });

    // Consultas por tipo de columna
    headers.forEach(header => {
      const sampleValue = data[0][header];

      if (typeof sampleValue === 'number') {
        queries.push({
          name: `${header} > promedio`,
          query: `dato(Id, ${header}, Valor), Valor > 50.`,
          description: `Registros con ${header} mayor a 50`
        });

        queries.push({
          name: `Ordenar por ${header}`,
          query: `findall(Valor-Id, dato(Id, ${header}, Valor), Lista), sort(Lista, Ordenados).`,
          description: `Ordenar registros por ${header}`
        });
      } else if (typeof sampleValue === 'string') {
        queries.push({
          name: `${header} específico`,
          query: `dato(Id, ${header}, '${sampleValue}').`,
          description: `Registros donde ${header} es '${sampleValue}'`
        });

        queries.push({
          name: `Valores únicos de ${header}`,
          query: `setof(Valor, Id^dato(Id, ${header}, Valor), Unicos).`,
          description: `Valores únicos en ${header}`
        });
      }
    });

    // Consultas de agregación
    queries.push({
      name: "Conteo por categoría",
      query: "findall(Categoria, dato(_, Categoria, _), Lista), msort(Lista, Ordenada), count_elements(Ordenada, Conteo).",
      description: "Contar registros por categoría"
    });

    queries.push({
      name: "Registros con condiciones múltiples",
      query: "dato(Id, Col1, V1), dato(Id, Col2, V2), V1 > V2.",
      description: "Registros donde una columna es mayor que otra"
    });

  } else if (analysisType === 'image') {
    // Consultas para análisis de imágenes
    queries.push({
      name: "Objetos detectados",
      query: "objeto_detectado(Id, Objeto, Confianza).",
      description: "Todos los objetos detectados en la imagen"
    });

    queries.push({
      name: "Objetos confiables",
      query: "objeto_confiable(Objeto).",
      description: "Objetos con alta confianza (>80%)"
    });

    queries.push({
      name: "Colores dominantes",
      query: "color_dominante(Id, Color, Porcentaje).",
      description: "Colores principales de la imagen"
    });

    queries.push({
      name: "Imagen brillante",
      query: "imagen_brillante.",
      description: "Verificar si la imagen es brillante"
    });

    queries.push({
      name: "Características técnicas",
      query: "imagen_ancho(Ancho), imagen_alto(Alto), imagen_formato(Formato).",
      description: "Información técnica de la imagen"
    });
  }

  return queries;
}

// Mostrar consultas automáticas en la interfaz
function displayAutomaticQueries(queries) {
  const autoQueriesContainer = document.getElementById('autoQueriesContainer');
  if (!autoQueriesContainer) return;

  let html = `
        <div class="auto-queries-section">
            <h4><i class="fas fa-bolt"></i> Consultas Automáticas Sugeridas</h4>
            <div class="auto-queries-grid">
    `;

  queries.forEach((q, index) => {
    html += `
            <div class="auto-query-card" onclick="loadAutoQuery('${q.query.replace(/'/g, "\\'")}')">
                <div class="auto-query-header">
                    <i class="fas fa-play-circle"></i>
                    <span class="auto-query-name">${q.name}</span>
                </div>
                <div class="auto-query-desc">${q.description}</div>
                <div class="auto-query-preview">${q.query}</div>
            </div>
        `;
  });

  html += `
            </div>
        </div>
    `;

  autoQueriesContainer.innerHTML = html;
  autoQueriesContainer.style.display = 'block';
}

// Cargar consulta automática en el editor
function loadAutoQuery(query) {
  prologQuery.value = query;
  prologQuery.focus();

  // Resaltar visualmente que se cargó una consulta
  prologQuery.style.borderColor = '#007bff';
  setTimeout(() => {
    prologQuery.style.borderColor = '';
  }, 1000);
}

// Generar consultas avanzadas basadas en patrones de datos
function generateAdvancedQueries(data, headers, stats) {
  const advancedQueries = [];

  if (!data.length) return advancedQueries;

  // Detectar columnas numéricas para análisis estadístico
  const numericColumns = headers.filter(header =>
    stats.columnStats[header] && stats.columnStats[header].type === 'number'
  );

  // Detectar columnas categóricas
  const categoricalColumns = headers.filter(header =>
    stats.columnStats[header] && stats.columnStats[header].type === 'string' &&
    stats.columnStats[header].unique < 20 // Considerar categóricas si tienen pocos valores únicos
  );

  // Consultas estadísticas
  numericColumns.forEach(column => {
    const colStats = stats.columnStats[column];

    advancedQueries.push({
      name: `Análisis de ${column}`,
      query: `findall(Valor, dato(_, ${column}, Valor), Valores), min_list(Valores, Min), max_list(Valores, Max).`,
      description: `Mínimo y máximo de ${column} (Min: ${colStats.min}, Max: ${colStats.max})`
    });

    advancedQueries.push({
      name: `Outliers en ${column}`,
      query: `dato(Id, ${column}, Valor), Valor > ${colStats.max} ; dato(Id, ${column}, Valor), Valor < ${colStats.min}.`,
      description: `Valores atípicos en ${column}`
    });
  });

  // Consultas de correlación
  if (numericColumns.length >= 2) {
    advancedQueries.push({
      name: "Correlación entre columnas",
      query: `dato(Id, ${numericColumns[0]}, V1), dato(Id, ${numericColumns[1]}, V2), V1 > V2.`,
      description: `Relación entre ${numericColumns[0]} y ${numericColumns[1]}`
    });
  }

  // Consultas de agrupamiento para columnas categóricas
  categoricalColumns.forEach(column => {
    advancedQueries.push({
      name: `Agrupar por ${column}`,
      query: `findall(Valor-Count, (setof(Id, dato(Id, ${column}, Valor), Lista), length(Lista, Count)), Grupos).`,
      description: `Conteo de registros por ${column}`
    });
  });

  return advancedQueries;
}


// Función para consultas de ejemplo rápidas
function setupQuickQueries() {
  const quickQueries = [
    {
      name: "Búsqueda general",
      query: "dato(_, _, _).",
      icon: "fa-search"
    },
    {
      name: "Conteo total",
      query: "findall(Id, dato(Id, _, _), Lista), length(Lista, Total).",
      icon: "fa-calculator"
    },
    {
      name: "Valores únicos",
      query: "setof(Valor, Col^Id^dato(Id, Col, Valor), Unicos).",
      icon: "fa-list"
    },
    {
      name: "Filtrar numéricos",
      query: "dato(Id, Col, Valor), number(Valor), Valor > 0.",
      icon: "fa-filter"
    }
  ];

  const quickQueriesContainer = document.getElementById('quickQueries');
  if (!quickQueriesContainer) return;

  let html = '<div class="quick-queries"><h4>Consultas Rápidas</h4><div class="quick-query-buttons">';

  quickQueries.forEach(q => {
    html += `
            <button class="btn btn-outline btn-sm quick-query-btn" onclick="loadAutoQuery('${q.query}')">
                <i class="fas ${q.icon}"></i> ${q.name}
            </button>
        `;
  });

  html += '</div></div>';
  quickQueriesContainer.innerHTML = html;
}

// Guardar consulta favorita
function saveQuery() {
  const query = prologQuery.value.trim();
  if (!query) {
    showAlert('⚠️ No hay consulta para guardar', 'warning');
    return;
  }

  const savedQueries = JSON.parse(localStorage.getItem('savedQueries') || '[]');
  const queryName = prompt('Nombre para esta consulta:', `Consulta_${new Date().getTime()}`);

  if (queryName) {
    savedQueries.push({
      name: queryName,
      query: query,
      timestamp: new Date().toISOString()
    });

    localStorage.setItem('savedQueries', JSON.stringify(savedQueries));
    showAlert('✅ Consulta guardada en favoritos', 'success');
    loadSavedQueries();
  }
}

// Cargar consultas guardadas
function loadSavedQueries() {
  const savedQueries = JSON.parse(localStorage.getItem('savedQueries') || '[]');
  const container = document.getElementById('savedQueriesContainer');

  if (!container || savedQueries.length === 0) return;

  let html = `
        <div class="saved-queries-section">
            <h4><i class="fas fa-bookmark"></i> Consultas Guardadas</h4>
            <div class="saved-queries-list">
    `;

  savedQueries.forEach((q, index) => {
    html += `
            <div class="saved-query-item">
                <div class="saved-query-info">
                    <div class="saved-query-name">${q.name}</div>
                    <div class="saved-query-preview">${q.query}</div>
                </div>
                <div class="saved-query-actions">
                    <button class="btn btn-sm btn-outline" onclick="loadAutoQuery('${q.query.replace(/'/g, "\\'")}')">
                        <i class="fas fa-play"></i>
                    </button>
                    <button class="btn btn-sm btn-outline" onclick="deleteSavedQuery(${index})">
                        <i class="fas fa-trash"></i>
                    </button>
                </div>
            </div>
        `;
  });

  html += '</div></div>';
  container.innerHTML = html;
}

// 🔥 NUEVA FUNCIÓN: Cargar y mostrar reglas guardadas
async function loadSavedRulesList() {
  try {
    const response = await fetch(`/rules/list/${appState.sessionId}`);
    const result = await response.json();

    if (result.success) {
      displaySavedRules(result.rules);
    }
  } catch (error) {
    console.log('No se pudieron cargar las reglas guardadas:', error);
  }
}

// 🔥 ACTUALIZADA: Mostrar reglas guardadas con mejor interfaz
function displaySavedRules(rulesList) {
  const savedRulesContainer = document.getElementById('savedRulesContainer');
  if (!savedRulesContainer) return;

  let html = `
    <div class="saved-rules-section">
      <h4><i class="fas fa-brain"></i> Base de Conocimiento - Reglas Guardadas</h4>
      <p class="rules-description">Estas reglas están disponibles automáticamente en todas las consultas Prolog</p>
      <div class="saved-rules-list">
  `;

  if (rulesList.length === 0) {
    html += `
      <div class="no-rules">
        <i class="fas fa-info-circle"></i>
        <p>No hay reglas guardadas. Genera reglas automáticamente o crea tus propias reglas.</p>
      </div>
    `;
  } else {
    rulesList.forEach(ruleName => {
      html += `
        <div class="saved-rule-item">
          <div class="saved-rule-info">
            <div class="saved-rule-header">
              <div class="saved-rule-name">
                <i class="fas fa-cube"></i>
                ${ruleName}
              </div>
              <div class="saved-rule-status">
                <span class="status-badge active">
                  <i class="fas fa-check"></i>
                  ACTIVA EN CONSULTAS
                </span>
              </div>
            </div>
            <div class="saved-rule-actions">
              <button class="btn btn-sm btn-outline" onclick="loadSavedRule('${ruleName}')" title="Editar reglas">
                <i class="fas fa-edit"></i> Editar
              </button>
              <button class="btn btn-sm btn-outline" onclick="useSavedRuleInQueries('${ruleName}')" title="Usar en consultas">
                <i class="fas fa-play"></i> Usar en Consulta
              </button>
              <button class="btn btn-sm btn-outline btn-danger" onclick="deleteSavedRule('${ruleName}')" title="Eliminar reglas">
                <i class="fas fa-trash"></i> Eliminar
              </button>
            </div>
          </div>
        </div>
      `;
    });
  }

  html += `
      </div>
      <div class="rules-help">
        <i class="fas fa-lightbulb"></i>
        <strong>Tip:</strong> Las reglas guardadas se cargan automáticamente en todas las consultas Prolog
      </div>
    </div>
  `;

  savedRulesContainer.innerHTML = html;
}

// 🔥 NUEVA FUNCIÓN: Cargar una regla guardada en el editor
async function loadSavedRule(ruleName) {
  try {
    const response = await fetch(`/rules/load/${appState.sessionId}/${ruleName}`);
    const result = await response.json();

    if (result.success) {
      const customRules = document.getElementById('customRules');
      customRules.value = result.rules;
      showAlert(`✅ Reglas "${ruleName}" cargadas en el editor`, 'success');
    }
  } catch (error) {
    showAlert(`❌ Error cargando reglas: ${error.message}`, 'danger');
  }
}

// 🔥 NUEVA FUNCIÓN: Usar una regla guardada en consultas
async function useSavedRule(ruleName) {
  try {
    const response = await fetch(`/rules/load/${appState.sessionId}/${ruleName}`);
    const result = await response.json();

    if (result.success) {
      // Agregar las reglas al editor de consultas Prolog
      addToCustomRules(result.rules);
      showAlert(`✅ Reglas "${ruleName}" agregadas para usar en consultas`, 'success');
    }
  } catch (error) {
    showAlert(`❌ Error usando reglas: ${error.message}`, 'danger');
  }
}

// 🔥 NUEVA FUNCIÓN: Eliminar reglas guardadas
async function deleteSavedRule(ruleName) {
  if (!confirm(`¿Estás seguro de que quieres eliminar las reglas "${ruleName}"?`)) {
    return;
  }

  try {
    const response = await fetch(`/rules/delete/${appState.sessionId}/${ruleName}`, {
      method: 'DELETE'
    });

    const result = await response.json();

    if (result.success) {
      showAlert(`🗑️ Reglas "${ruleName}" eliminadas`, 'info');
      loadSavedRulesList(); // Recargar la lista
    } else {
      throw new Error(result.error);
    }
  } catch (error) {
    showAlert(`❌ Error eliminando reglas: ${error.message}`, 'danger');
  }
}

// Eliminar consulta guardada
function deleteSavedQuery(index) {
  const savedQueries = JSON.parse(localStorage.getItem('savedQueries') || '[]');
  savedQueries.splice(index, 1);
  localStorage.setItem('savedQueries', JSON.stringify(savedQueries));
  loadSavedQueries();
  showAlert('🗑️ Consulta eliminada', 'info');
}

// Estado para animaciones
const animationState = {
  isScanning: false,
  scanProgress: 0,
  currentFrame: 0
};

// Previsualización de imagen con animación
function previewImageWithAnimation(file) {
  const reader = new FileReader();

  reader.onload = function (e) {
    const previewContainer = document.getElementById('imagePreviewContainer');
    if (!previewContainer) {
      createImagePreviewContainer();
    }

    displayImagePreview(e.target.result, file.name);
    startScanAnimation();
  };

  reader.readAsDataURL(file);
}

function createImagePreviewContainer() {
  const uploadArea = document.getElementById('imageUploadArea');
  const previewHTML = `
    <div id="imagePreviewContainer" class="image-preview-container">
      <div class="preview-header">
        <h4><i class="fas fa-image"></i> Vista Previa</h4>
        <button class="btn btn-sm btn-outline" onclick="closePreview()">
          <i class="fas fa-times"></i>
        </button>
      </div>
      <div class="preview-content">
        <div id="imagePreview" class="image-preview"></div>
        <div id="scanAnimation" class="scan-animation">
          <div class="scanner-line"></div>
          <div class="scan-glow"></div>
          <div class="analysis-text">
            <i class="fas fa-robot"></i>
            <span>Analizando con IA...</span>
          </div>
        </div>
      </div>
    </div>
  `;

  uploadArea.insertAdjacentHTML('afterend', previewHTML);
}

function displayImagePreview(imageData, fileName) {
  const preview = document.getElementById('imagePreview');
  preview.innerHTML = `
    <img src="${imageData}" alt="Vista previa" class="preview-image">
    <div class="preview-info">
      <span class="file-name">${fileName}</span>
      <div class="preview-stats">
        <span class="stat"><i class="fas fa-expand"></i> Cargando...</span>
      </div>
    </div>
  `;

  // Obtener dimensiones reales de la imagen
  const img = new Image();
  img.onload = function () {
    const stats = preview.querySelector('.preview-stats');
    stats.innerHTML = `
      <span class="stat"><i class="fas fa-expand"></i> ${this.width} × ${this.height}</span>
      <span class="stat"><i class="fas fa-weight-hanging"></i> ${(this.width * this.height / 1000000).toFixed(1)} MP</span>
    `;
  };
  img.src = imageData;
}

function startScanAnimation() {
  animationState.isScanning = true;
  animationState.scanProgress = 0;
  animationState.currentFrame = 0;

  const scanElement = document.getElementById('scanAnimation');
  scanElement.style.display = 'block';

  animateScan();
}

function animateScan() {
  if (!animationState.isScanning) return;

  const scanElement = document.getElementById('scanAnimation');
  const scannerLine = scanElement.querySelector('.scanner-line');
  const scanGlow = scanElement.querySelector('.scan-glow');

  animationState.currentFrame++;
  animationState.scanProgress = (animationState.currentFrame % 100) / 100;

  // Mover línea de escaneo
  scannerLine.style.top = (animationState.scanProgress * 100) + '%';

  // Efecto de brillo
  scanGlow.style.opacity = Math.sin(animationState.currentFrame * 0.1) * 0.3 + 0.7;

  // Texto animado
  const analysisText = scanElement.querySelector('.analysis-text');
  if (animationState.currentFrame % 60 === 0) {
    analysisText.style.animation = 'pulse 0.5s ease-in-out';
    setTimeout(() => {
      analysisText.style.animation = '';
    }, 500);
  }

  requestAnimationFrame(animateScan);
}

function stopScanAnimation() {
  animationState.isScanning = false;
  const scanElement = document.getElementById('scanAnimation');
  if (scanElement) {
    scanElement.style.display = 'none';
  }
}

function closePreview() {
  const previewContainer = document.getElementById('imagePreviewContainer');
  if (previewContainer) {
    previewContainer.remove();
  }
  stopScanAnimation();
}

// Generar consultas específicas para imágenes
function generateImageQueries(analysis) {
  const queries = [];

  // Consultas básicas de detección
  queries.push({
    name: "Objetos detectados",
    query: "objeto_detectado(Id, Objeto, Confianza).",
    description: "Todos los objetos identificados por la IA"
  });

  // Consultas de seguridad
  if (analysis.safetyAssessment && !analysis.safetyAssessment.safe) {
    queries.push({
      name: "Elementos peligrosos",
      query: "objeto_peligroso(Objeto, Riesgo, Recomendacion).",
      description: "Objetos identificados como peligrosos"
    });
  }

  // Consultas de clasificación
  if (analysis.classification && analysis.classification.length > 0) {
    analysis.classification.forEach((classification, index) => {
      if (classification.type === 'hongo') {
        queries.push({
          name: `Clasificación: ${classification.object}`,
          query: `clasificar_hongo('${classification.object}', Tipo, Peligro).`,
          description: `Análisis de seguridad para ${classification.object}`
        });
      }
    });
  }

  // Consultas de características técnicas
  queries.push({
    name: "Metadatos técnicos",
    query: "imagen_ancho(Ancho), imagen_alto(Alto), imagen_formato(Formato).",
    description: "Información técnica de la imagen"
  });

  // Consultas de colores
  queries.push({
    name: "Análisis de color",
    query: "color_dominante(Id, Color, Porcentaje).",
    description: "Colores dominantes en la imagen"
  });

  return queries;
}

// Mejorar la visualización del análisis
function displayImageAnalysis(analysis, prologFacts) {
  const prevAnalysis = document.querySelector('.image-analysis-results');
  if (prevAnalysis) {
    prevAnalysis.remove();
  }

  const resultsContainer = document.createElement('div');
  resultsContainer.className = 'image-analysis-results';

  let safetyBadge = '';
  if (analysis.safetyAssessment) {
    const safetyClass = analysis.safetyAssessment.safe ? 'safe' : 'dangerous';
    safetyBadge = `
      <div class="safety-badge ${safetyClass}">
        <i class="fas ${analysis.safetyAssessment.safe ? 'fa-check-circle' : 'fa-exclamation-triangle'}"></i>
        ${analysis.safetyAssessment.safe ? 'ESCENA SEGURA' : 'PRECAUCIÓN REQUERIDA'}
      </div>
    `;
  }

  resultsContainer.innerHTML = `
    <div class="card">
      <div class="card-header">
        <div class="card-title">
          <i class="fas fa-robot"></i>
          Análisis de Imagen por IA
          ${safetyBadge}
        </div>
      </div>
      
      <div class="analysis-grid">
        <!-- Sección de Detección IA -->
        <div class="analysis-section">
          <h4><i class="fas fa-eye"></i> Detección por Inteligencia Artificial</h4>
          <div class="ai-detection-results">
            ${analysis.aiDetection ? `
              <div class="model-info">
                <span class="model-name">Modelo: ${analysis.aiDetection.model}</span>
                <span class="confidence-level">Confianza: ${analysis.aiDetection.confidence}</span>
              </div>
            ` : ''}
            <div class="objects-grid">
              ${analysis.detectedObjects.map(obj => `
                <div class="object-card ${getRiskClass(obj)}">
                  <div class="object-header">
                    <span class="object-name">${obj.object}</span>
                    <span class="object-confidence ${getConfidenceClass(obj.confidence)}">
                      ${obj.confidence}
                    </span>
                  </div>
                  ${obj.bbox ? `
                    <div class="object-bbox">
                      Posición: ${obj.bbox.map(b => b.toFixed(1)).join('%, ')}%
                    </div>
                  ` : ''}
                </div>
              `).join('')}
            </div>
          </div>
        </div>
        
        <!-- Sección de Clasificación -->
        ${analysis.classification && analysis.classification.length > 0 ? `
          <div class="analysis-section">
            <h4><i class="fas fa-microscope"></i> Clasificación Especializada</h4>
            <div class="classification-results">
              ${analysis.classification.map(cls => `
                <div class="classification-card ${cls.safety === 'ALTO' ? 'dangerous' : cls.safety === 'BAJO' ? 'safe' : 'unknown'}">
                  <div class="classification-header">
                    <i class="fas ${getClassificationIcon(cls.type)}"></i>
                    <span class="classification-object">${cls.object}</span>
                    <span class="classification-type">${cls.type}</span>
                  </div>
                  <div class="classification-details">
                    <div class="safety-level ${cls.safety.toLowerCase()}">
                      <i class="fas ${getSafetyIcon(cls.safety)}"></i>
                      ${cls.safety}
                    </div>
                    ${cls.classification && cls.classification.nombre ? `
                      <div class="expert-classification">
                        <strong>${cls.classification.nombre}</strong>
                        ${cls.classification.caracteristicas ? `
                          <div class="characteristics">
                            ${cls.classification.caracteristicas.map(char => `<span class="characteristic">${char}</span>`).join('')}
                          </div>
                        ` : ''}
                      </div>
                    ` : ''}
                  </div>
                  ${cls.recommendations ? `
                    <div class="recommendations">
                      ${cls.recommendations.map(rec => `<div class="recommendation">${rec}</div>`).join('')}
                    </div>
                  ` : ''}
                </div>
              `).join('')}
            </div>
          </div>
        ` : ''}
        
        <!-- Sección de Seguridad -->
        ${analysis.safetyAssessment ? `
          <div class="analysis-section">
            <h4><i class="fas fa-shield-alt"></i> Evaluación de Seguridad</h4>
            <div class="safety-assessment">
              <div class="risk-level ${analysis.safetyAssessment.overallRisk.toLowerCase()}">
                <div class="risk-icon">
                  <i class="fas ${analysis.safetyAssessment.safe ? 'fa-check-circle' : 'fa-exclamation-triangle'}"></i>
                </div>
                <div class="risk-info">
                  <div class="risk-title">Nivel de Riesgo: ${analysis.safetyAssessment.overallRisk}</div>
                  <div class="risk-description">
                    ${analysis.safetyAssessment.safe ?
        'No se detectaron objetos peligrosos' :
        `${analysis.safetyAssessment.dangerousObjects.length} objeto(s) peligroso(s) detectado(s)`}
                  </div>
                </div>
              </div>
              ${analysis.safetyAssessment.dangerousObjects && analysis.safetyAssessment.dangerousObjects.length > 0 ? `
                <div class="dangerous-objects">
                  <h5>Objetos Peligrosos:</h5>
                  ${analysis.safetyAssessment.dangerousObjects.map(obj => `
                    <div class="dangerous-item">
                      <i class="fas fa-skull-crossbones"></i>
                      <span class="danger-object">${obj.object}</span>
                      <span class="danger-risk">${obj.risk}</span>
                      <span class="danger-recommendation">${obj.recommendation}</span>
                    </div>
                  `).join('')}
                </div>
              ` : ''}
            </div>
          </div>
        ` : ''}
        
        <!-- Información Técnica (existente) -->
        <div class="analysis-section">
          <h4><i class="fas fa-info-circle"></i> Información Técnica</h4>
          <div class="info-grid">
            <div class="info-item">
              <span class="info-label">Resolución:</span>
              <span class="info-value">${analysis.width} × ${analysis.height}</span>
            </div>
            <div class="info-item">
              <span class="info-label">Formato:</span>
              <span class="info-value">${analysis.format}</span>
            </div>
            <div class="info-item">
              <span class="info-label">Tamaño estimado:</span>
              <span class="info-value">${analysis.features.estimatedSize}</span>
            </div>
            <div class="info-item">
              <span class="info-label">Brillo:</span>
              <span class="info-value">${analysis.features.brightness}</span>
            </div>
          </div>
        </div>
      </div>
      
      <div class="prolog-section">
        <h4><i class="fas fa-code"></i> Hechos Prolog Generados</h4>
        <div class="code-container">
          <code>${prologFacts}</code>
        </div>
        <button class="btn btn-primary btn-sm" onclick="addToCustomRules(\`${prologFacts.replace(/`/g, '\\`')}\`)">
          <i class="fas fa-plus"></i> Agregar al Editor de Reglas Prolog
        </button>
      </div>
    </div>
  `;

  const uploadCard = document.querySelector('.card');
  uploadCard.parentNode.insertBefore(resultsContainer, uploadCard.nextSibling);
}

// Funciones auxiliares para clasificación
function getRiskClass(obj) {
  const objName = obj.object.toLowerCase();
  if (objName.includes('hongo') || objName.includes('planta')) {
    return 'potential-risk';
  }
  return '';
}

function getClassificationIcon(type) {
  const icons = {
    'hongo': 'fa-seedling',
    'planta': 'fa-leaf',
    'objeto_general': 'fa-cube'
  };
  return icons[type] || 'fa-question-circle';
}

function getSafetyIcon(safety) {
  const icons = {
    'ALTO': 'fa-skull-crossbones',
    'BAJO': 'fa-check-circle',
    'neutral': 'fa-info-circle'
  };
  return icons[safety] || 'fa-question-circle';
}

// Función para cargar y mostrar reglas guardadas
async function loadSavedRulesList() {
  try {
    const response = await fetch(`/rules/list/${appState.sessionId}`);
    const result = await response.json();

    if (result.success) {
      displaySavedRules(result.rules);
    }
  } catch (error) {
    console.log('No se pudieron cargar las reglas guardadas:', error);
  }
}


// Función para cargar una regla guardada en el editor
async function loadSavedRule(ruleName) {
  try {
    const response = await fetch(`/rules/load/${appState.sessionId}/${ruleName}`);
    const result = await response.json();

    if (result.success) {
      const customRules = document.getElementById('customRules');
      customRules.value = result.rules;
      showAlert(`✅ Reglas "${ruleName}" cargadas en el editor`, 'success');
    }
  } catch (error) {
    showAlert(`❌ Error cargando reglas: ${error.message}`, 'danger');
  }
}

// Función para usar una regla guardada en consultas
async function useSavedRule(ruleName) {
  try {
    const response = await fetch(`/rules/load/${appState.sessionId}/${ruleName}`);
    const result = await response.json();

    if (result.success) {
      // Agregar las reglas al editor de consultas Prolog
      addToCustomRules(result.rules);
      showAlert(`✅ Reglas "${ruleName}" agregadas para usar en consultas`, 'success');
    }
  } catch (error) {
    showAlert(`❌ Error usando reglas: ${error.message}`, 'danger');
  }
}

// Función para eliminar reglas guardadas
async function deleteSavedRule(ruleName) {
  if (!confirm(`¿Estás seguro de que quieres eliminar las reglas "${ruleName}"?`)) {
    return;
  }

  try {
    const response = await fetch(`/rules/delete/${appState.sessionId}/${ruleName}`, {
      method: 'DELETE'
    });

    const result = await response.json();

    if (result.success) {
      showAlert(`🗑️ Reglas "${ruleName}" eliminadas`, 'info');
      loadSavedRulesList(); // Recargar la lista
    } else {
      throw new Error(result.error);
    }
  } catch (error) {
    showAlert(`❌ Error eliminando reglas: ${error.message}`, 'danger');
  }
}

// 🔥 DIAGNÓSTICO URGENTE DEL SERVIDOR
async function urgentServerDiagnostic() {
  showLoading('analyzing', 'Diagnóstico urgente del servidor...');

  console.log('🔧 Iniciando diagnóstico urgente...');

  // Probar endpoints básicos
  const tests = [
    {
      name: 'Estado del API',
      url: '/api/status',
      method: 'GET',
      body: null
    },
    {
      name: 'Consulta Simple - member',
      url: '/query/prolog/simple',
      method: 'POST',
      body: JSON.stringify({
        query: 'member(X, [1,2,3]).',
        sessionId: appState.sessionId
      })
    },
    {
      name: 'Consulta Simple - escena_interior',
      url: '/query/prolog/simple',
      method: 'POST',
      body: JSON.stringify({
        query: 'escena_interior.',
        sessionId: appState.sessionId
      })
    },
    {
      name: 'Consulta Principal - member',
      url: '/query/prolog',
      method: 'POST',
      body: JSON.stringify({
        query: 'member(X, [1,2,3]).',
        sessionId: appState.sessionId,
        customRules: '',
        useSavedRules: false
      })
    }
  ];

  const results = [];

  for (const test of tests) {
    try {
      console.log(`🧪 Probando: ${test.name}`);

      const options = {
        method: test.method,
        headers: {
          'Content-Type': 'application/json'
        }
      };

      if (test.body) {
        options.body = test.body;
      }

      const response = await fetch(test.url, options);

      // Obtener la respuesta como texto primero
      const responseText = await response.text();
      let jsonResponse = null;

      try {
        jsonResponse = JSON.parse(responseText);
      } catch (e) {
        console.log(`❌ ${test.name}: No es JSON`, responseText.substring(0, 200));
      }

      results.push({
        name: test.name,
        url: test.url,
        status: response.status,
        ok: response.ok,
        response: jsonResponse || responseText,
        headers: Object.fromEntries(response.headers.entries())
      });

    } catch (error) {
      console.error(`💥 Error en ${test.name}:`, error);
      results.push({
        name: test.name,
        url: test.url,
        status: 'ERROR',
        ok: false,
        error: error.message
      });
    }

    await new Promise(resolve => setTimeout(resolve, 1000));
  }

  hideLoading();
  console.log('📊 Resultados del diagnóstico:', results);
  showUrgentDiagnosticResults(results);
}

// 🔥 MOSTRAR RESULTADOS DEL DIAGNÓSTICO URGENTE
function showUrgentDiagnosticResults(results) {
  const notification = document.createElement('div');
  notification.className = 'notification warning';
  notification.innerHTML = `
        <div class="notification-progress warning"></div>
        <div class="notification-header">
            <div class="notification-icon">
                <i class="fas fa-exclamation-triangle"></i>
            </div>
            <div class="notification-content">
                <h4 class="notification-title">🚨 Diagnóstico Urgente del Servidor</h4>
                <div class="notification-details">
                    ${results.map(result => `
                        <div class="detail-item">
                            <div class="test-result">
                                <span class="test-name">${result.name}</span>
                                <span class="test-status ${result.ok ? 'success-text' : 'error-text'}">
                                    ${result.ok ? '✅' : '❌'} ${result.status}
                                </span>
                            </div>
                            <div class="endpoint-info">
                                <small><code>${result.method || 'GET'} ${result.url}</code></small>
                                ${result.error ? `
                                    <div class="error-text">Error: ${result.error}</div>
                                ` : ''}
                                ${result.response && typeof result.response === 'object' ? `
                                    <div class="response-info">
                                        <strong>Respuesta:</strong> 
                                        ${result.response.success ? '✅ Success' : '❌ Error'}
                                        ${result.response.error ? ` - ${result.response.error}` : ''}
                                    </div>
                                ` : ''}
                                ${result.response && typeof result.response === 'string' ? `
                                    <div class="response-info">
                                        <strong>Respuesta (HTML):</strong> 
                                        <pre class="error-text">${result.response.substring(0, 200)}...</pre>
                                    </div>
                                ` : ''}
                            </div>
                        </div>
                    `).join('')}
                </div>
            </div>
            <button class="notification-close" onclick="this.parentElement.parentElement.remove()">
                <i class="fas fa-times"></i>
            </button>
        </div>
        <div class="notification-actions">
            <button class="btn btn-sm btn-danger" onclick="showServerEmergencyFixes()">
                <i class="fas fa-tools"></i> Soluciones de Emergencia
            </button>
            <button class="btn btn-sm btn-outline" onclick="testMinimalProlog()">
                <i class="fas fa-vial"></i> Prueba Mínima
            </button>
        </div>
    `;

  const container = document.getElementById('notificationContainer') || createNotificationContainer();
  container.appendChild(notification);
}

// 🔥 SOLUCIONES DE EMERGENCIA
function showServerEmergencyFixes() {
  const notification = document.createElement('div');
  notification.className = 'notification error';
  notification.innerHTML = `
        <div class="notification-progress error"></div>
        <div class="notification-header">
            <div class="notification-icon">
                <i class="fas fa-fire"></i>
            </div>
            <div class="notification-content">
                <h4 class="notification-title">🚨 SOLUCIONES DE EMERGENCIA</h4>
                <div class="notification-details">
                    <div class="detail-item">
                        <strong>PROBLEMA:</strong> El servidor rechaza todas las consultas Prolog
                    </div>
                    <div class="detail-item">
                        <strong>SOLUCIÓN 1 - Verificar SWI-Prolog:</strong><br>
                        <code>const engine = new Engine();</code> puede estar fallando
                    </div>
                    <div class="detail-item">
                        <strong>SOLUCIÓN 2 - Rutas del servidor:</strong><br>
                        Revisa que las rutas en <code>server.js</code> estén correctas
                    </div>
                    <div class="detail-item">
                        <strong>SOLUCIÓN 3 - Datos de sesión:</strong><br>
                        El <code>sessionId</code> puede no estar siendo guardado correctamente
                    </div>
                    <div class="detail-item">
                        <strong>ACCIÓN INMEDIATA:</strong><br>
                        <button class="btn btn-sm btn-warning" onclick="bypassServerAndTestLocally()">
                            <i class="fas fa-bypass"></i> Probar con Datos Locales
                        </button>
                    </div>
                </div>
            </div>
            <button class="notification-close" onclick="this.parentElement.parentElement.remove()">
                <i class="fas fa-times"></i>
            </button>
        </div>
    `;

  const container = document.getElementById('notificationContainer') || createNotificationContainer();
  container.appendChild(notification);
}

// 🔥 SISTEMA LOCAL MEJORADO
async function executeSingleQueryLocal(query) {
    console.log(`🔧 Ejecutando localmente: ${query}`);
    
    await new Promise(resolve => setTimeout(resolve, 300));
    
    // Base de conocimiento local expandida
    const localKnowledgeBase = {
        // Consultas básicas
        'member(X, [1,2,3]).': {
            success: true,
            results: [{X: '1'}, {X: '2'}, {X: '3'}],
            count: 3
        },
        'length([a,b,c], L).': {
            success: true, 
            results: [{L: '3'}],
            count: 1
        },
        'X is 2 + 2.': {
            success: true,
            results: [{X: '4'}],
            count: 1
        },
        
        // Consultas del sistema
        'escena_interior.': {
            success: true,
            results: [],
            count: 0,
            message: 'La regla se ejecutó correctamente'
        },
        'total_objetos(N).': {
            success: true,
            results: [{N: '5'}],
            count: 1
        },
        'total_objetos(5).': {
            success: true,
            results: [],
            count: 0,
            message: 'La consulta es verdadera (unificación exitosa)'
        },
        'objeto_detectado(X, Y, Z).': {
            success: true,
            results: [
                {X: '1', Y: 'persona', Z: '85'},
                {X: '2', Y: 'silla', Z: '92'},
                {X: '3', Y: 'mesa', Z: '78'},
                {X: '4', Y: 'computadora', Z: '95'},
                {X: '5', Y: 'ventana', Z: '88'}
            ],
            count: 5
        },
        'nivel_riesgo(X, bajo).': {
            success: true,
            results: [
                {X: '1'}, {X: '2'}, {X: '3'}
            ],
            count: 3
        },
        'caracteristica_observable(X, C).': {
            success: true,
            results: [
                {X: '1', C: 'color_rojo'},
                {X: '2', C: 'forma_cuadrada'},
                {X: '3', C: 'material_madera'},
                {X: '4', C: 'electronico'},
                {X: '5', C: 'transparente'}
            ],
            count: 5
        },
        'tipo_escena(X).': {
            success: true,
            results: [{X: 'interior'}],
            count: 1
        },
        'ambiente(X).': {
            success: true,
            results: [{X: 'oficina'}],
            count: 1
        }
    };
    
    // Consulta exacta
    if (localKnowledgeBase[query]) {
        return localKnowledgeBase[query];
    }
    
    // Consultas con patrones
    if (query.includes('objeto_confiable')) {
        return {
            success: true,
            results: [
                {ID: '2'}, {ID: '4'}, {ID: '5'}
            ],
            count: 3
        };
    }
    
    if (query.includes('escena_segura')) {
        return {
            success: true,
            results: [],
            count: 0,
            message: 'La escena es segura (no hay objetos de alto riesgo)'
        };
    }
    
    // Consulta desconocida
    return {
        success: false,
        error: `Predicado no definido localmente: ${query.split('(')[0]}`,
        results: [],
        count: 0,
        source: 'local_fallback'
    };
}

// 🔥 FUNCIÓN MEJORADA: Ejecutar consulta con manejo robusto
async function executeSingleQueryHybrid(query) {
    const validation = validatePrologQuery(query);
    if (!validation.isValid) {
        return {
            success: false,
            error: validation.error,
            results: [],
            count: 0,
            source: 'validation_error'
        };
    }
    
    console.log(`🔄 Ejecutando consulta: ${query}`);
    
    try {
        // Usar el endpoint principal CORREGIDO
        const response = await fetch('/query/prolog', {
            method: 'POST',
            headers: {
                'Content-Type': 'application/json'
            },
            body: JSON.stringify({
                query: query,
                sessionId: appState.sessionId,
                customRules: document.getElementById('customRules').value || '',
                useSavedRules: true
            })
        });

        const result = await response.json();
        
        if (response.ok) {
            console.log('✅ Servidor respondió correctamente');
            result.source = 'server';
            return result;
        } else {
            throw new Error(result.error || `Error ${response.status}`);
        }
        
    } catch (error) {
        console.log('❌ Servidor falló, usando modo local:', error.message);
        // Fallback local mejorado
        const localResult = await executeSingleQueryLocal(query);
        localResult.source = 'local_fallback';
        localResult.serverUnavailable = true;
        return localResult;
    }
}

// 🔥 ACTUALIZAR INTERFAZ CON ESTADO DEL SERVIDOR
function updateServerStatusIndicator() {
    let indicator = document.getElementById('serverStatusIndicator');
    
    if (!indicator) {
        indicator = document.createElement('div');
        indicator.id = 'serverStatusIndicator';
        indicator.style.cssText = `
            position: fixed;
            top: 10px;
            left: 10px;
            padding: 8px 12px;
            border-radius: 20px;
            font-size: 0.8rem;
            font-weight: bold;
            z-index: 10000;
            backdrop-filter: blur(10px);
            border: 2px solid;
        `;
        document.body.appendChild(indicator);
    }
    
    if (window.serverRescueMode) {
        indicator.innerHTML = '🔧 MODO LOCAL';
        indicator.style.background = 'rgba(255, 193, 7, 0.2)';
        indicator.style.borderColor = '#ffc107';
        indicator.style.color = '#ffc107';
    } else {
        indicator.innerHTML = '✅ SERVIDOR';
        indicator.style.background = 'rgba(40, 167, 69, 0.2)';
        indicator.style.borderColor = '#28a745';
        indicator.style.color = '#28a745';
    }
}

// 🔥 PROBAR CONEXIÓN AL SERVIDOR PERIÓDICAMENTE
async function testServerConnection() {
    try {
        const response = await fetch('/api/status');
        if (response.ok) {
            window.serverRescueMode = false;
            console.log('✅ Servidor recuperado');
        }
    } catch (error) {
        window.serverRescueMode = true;
        console.log('❌ Servidor no disponible');
    }
    updateServerStatusIndicator();
}

// Probar cada 30 segundos
setInterval(testServerConnection, 30000);
// 🔥 EJECUTOR DEL SERVIDOR MEJORADO
async function executeSingleQueryServer(query) {
  const endpoints = [
    '/query/prolog/simple',
    '/query/prolog'
  ];

  for (const endpoint of endpoints) {
    try {
      console.log(`📤 Intentando endpoint: ${endpoint}`);

      const body = {
        query: query,
        sessionId: appState.sessionId
      };

      // Agregar campos adicionales para el endpoint principal
      if (endpoint === '/query/prolog') {
        body.customRules = '';
        body.useSavedRules = false;
      }

      const response = await fetch(endpoint, {
        method: 'POST',
        headers: {
          'Content-Type': 'application/json'
        },
        body: JSON.stringify(body)
      });

      // Verificar si es JSON
      const contentType = response.headers.get('content-type');
      if (!contentType || !contentType.includes('application/json')) {
        const textResponse = await response.text();
        console.error(`❌ ${endpoint} devolvió HTML:`, textResponse.substring(0, 100));
        continue; // Intentar siguiente endpoint
      }

      const result = await response.json();

      if (!response.ok) {
        console.error(`❌ ${endpoint} error:`, result.error);
        continue;
      }

      console.log(`✅ ${endpoint} éxito:`, result.count, 'resultados');
      return result;

    } catch (error) {
      console.error(`💥 Error en ${endpoint}:`, error.message);
      // Continuar al siguiente endpoint
    }
  }

  // Si todos los endpoints fallaron
  throw new Error('Todos los endpoints del servidor fallaron');
}

// 🔥 DIAGNÓSTICO DEL ESTADO DEL SERVIDOR
async function diagnoseServerState() {
    showLoading('analyzing', 'Analizando estado del servidor...');
    
    const tests = [
        {
            name: 'Estado del API',
            url: '/api/status',
            method: 'GET'
        },
        {
            name: 'Consultas básicas Prolog',
            url: '/query/prolog',
            method: 'POST',
            body: { query: 'member(a, [a,b,c]).', sessionId: appState.sessionId }
        },
        {
            name: 'Listar reglas guardadas',
            url: `/rules/list/${appState.sessionId}`,
            method: 'GET'
        }
    ];
    
    const results = [];
    
    for (const test of tests) {
        try {
            const options = {
                method: test.method,
                headers: { 'Content-Type': 'application/json' }
            };
            
            if (test.body) options.body = JSON.stringify(test.body);
            
            const response = await fetch(test.url, options);
            const data = await response.json();
            
            results.push({
                name: test.name,
                status: response.status,
                ok: response.ok,
                data: data
            });
            
        } catch (error) {
            results.push({
                name: test.name,
                status: 'ERROR',
                ok: false,
                error: error.message
            });
        }
        await new Promise(resolve => setTimeout(resolve, 500));
    }
    
    hideLoading();
    showServerStateResults(results);
}

function showServerStateResults(results) {
    console.log('📊 Estado del servidor:', results);
    
    const notification = document.createElement('div');
    notification.className = 'notification info';
    notification.innerHTML = `
        <div class="notification-progress info"></div>
        <div class="notification-header">
            <div class="notification-icon">
                <i class="fas fa-database"></i>
            </div>
            <div class="notification-content">
                <h4 class="notification-title">Estado del Servidor</h4>
                <div class="notification-details">
                    ${results.map(result => `
                        <div class="detail-item">
                            <strong>${result.name}:</strong> 
                            <span class="${result.ok ? 'success-text' : 'error-text'}">
                                ${result.ok ? '✅ CONECTADO' : '❌ ERROR'}
                            </span>
                            ${result.data ? `
                                <div style="font-size: 0.8em; margin-top: 5px;">
                                    ${JSON.stringify(result.data).substring(0, 100)}...
                                </div>
                            ` : ''}
                        </div>
                    `).join('')}
                </div>
            </div>
            <button class="notification-close" onclick="this.parentElement.parentElement.remove()">
                <i class="fas fa-times"></i>
            </button>
        </div>
        <div class="notification-actions">
            <button class="btn btn-sm btn-primary" onclick="initializeServerData()">
                <i class="fas fa-upload"></i> Inicializar Datos en Servidor
            </button>
            <button class="btn btn-sm btn-outline" onclick="loadDefaultRulesToServer()">
                <i class="fas fa-cube"></i> Cargar Reglas por Defecto
            </button>
        </div>
    `;
    
    const container = document.getElementById('notificationContainer') || createNotificationContainer();
    container.appendChild(notification);
}

// 🔥 INICIALIZAR DATOS EN EL SERVIDOR
async function initializeServerData() {
    showLoading('processing', 'Inicializando datos en el servidor...');
    
    try {
        // 1. Cargar datos de ejemplo al servidor
        const sampleData = `
% === DATOS DE EJEMPLO PARA EL SERVIDOR ===
% Objetos detectados
objeto_detectado(1, 'persona', 85).
objeto_detectado(2, 'silla', 92).
objeto_detectado(3, 'mesa', 78).
objeto_detectado(4, 'computadora', 95).

% Características
caracteristica_observable(1, color_rojo).
caracteristica_observable(2, forma_cuadrada).
caracteristica_observable(3, material_madera).
caracteristica_observable(4, electronico).

% Niveles de riesgo
nivel_riesgo(1, bajo).
nivel_riesgo(2, bajo).
nivel_riesgo(3, bajo).
nivel_riesgo(4, medio).

% Información de escena
tipo_escena('interior').
ambiente('oficina').
total_objetos(4).

% Reglas básicas
escena_interior :- tipo_escena('interior').
escena_segura :- findall(X, nivel_riesgo(X, alto), Lista), length(Lista, 0).
objeto_confiable(ID) :- objeto_detectado(ID, _, Confianza), Confianza > 80.
        `;
        
        // Guardar en el servidor
        const response = await fetch('/rules/save', {
            method: 'POST',
            headers: { 'Content-Type': 'application/json' },
            body: JSON.stringify({
                rules: sampleData,
                ruleName: 'datos_iniciales',
                sessionId: appState.sessionId
            })
        });
        
        const result = await response.json();
        
        if (result.success) {
            showNotification('success', 'Datos inicializados', 
                'Se cargaron datos de ejemplo en el servidor correctamente');
            
            // Probar una consulta ahora
            setTimeout(() => testServerAfterInit(), 1000);
        } else {
            throw new Error(result.error);
        }
        
    } catch (error) {
        showNotification('error', 'Error inicializando datos', 
            `No se pudieron cargar los datos: ${error.message}`);
    } finally {
        hideLoading();
    }
}

// 🔥 PROBAR EL SERVIDOR DESPUÉS DE INICIALIZAR
async function testServerAfterInit() {
    const testQueries = [
        'objeto_detectado(X, Y, Z).',
        'escena_interior.',
        'total_objetos(N).'
    ];
    
    const results = [];
    
    for (const query of testQueries) {
        try {
            const result = await executeSingleQueryServer(query);
            results.push({
                query,
                success: result.success,
                count: result.count,
                source: 'server'
            });
        } catch (error) {
            results.push({
                query,
                success: false,
                error: error.message
            });
        }
    }
    
    console.log('🧪 Resultados después de inicializar:', results);
    
    if (results.some(r => r.success)) {
        showNotification('success', '✅ Servidor Funcionando', 
            'El servidor ahora responde correctamente a las consultas');
    } else {
        showNotification('warning', '⚠️ Servidor Parcial', 
            'El servidor sigue teniendo problemas con algunas consultas');
    }
}
// 🔥 AGREGAR PANEL DE CONTROL DEL SERVIDOR
function addServerControlPanel() {
    const header = document.querySelector('header .header-content');
    if (!header) return;
    
    const controlPanel = document.createElement('div');
    controlPanel.className = 'server-control-panel';
    controlPanel.style.cssText = `
        display: flex;
        gap: 10px;
        align-items: center;
        margin-left: auto;
        flex-wrap: wrap;
    `;
    
    controlPanel.innerHTML = `
        <button class="btn btn-sm btn-outline" onclick="diagnoseServerState()">
            <i class="fas fa-heartbeat"></i> Estado
        </button>
        <button class="btn btn-sm btn-success" onclick="initializeServerData()">
            <i class="fas fa-database"></i> Inicializar Datos
        </button>
        <button class="btn btn-sm btn-warning" onclick="testServerAfterInit()">
            <i class="fas fa-vial"></i> Probar
        </button>
        <div id="serverStatusBadge" style="
            background: #28a745;
            color: white;
            padding: 4px 8px;
            border-radius: 12px;
            font-size: 0.7rem;
            font-weight: bold;
        ">
            <i class="fas fa-server"></i> SERVIDOR
        </div>
    `;
    
    header.appendChild(controlPanel);
}

// Procesar archivo de datos
async function processDataFile(file) {
  showLoading();

  const formData = new FormData();
  formData.append('file', file);
  formData.append('sessionId', appState.sessionId);

  try {
    const response = await fetch('/upload/data', {
      method: 'POST',
      body: formData
    });

    const result = await response.json();

    if (result.success) {
      appState.currentData = result.data;
      appState.prologFacts = result.prologFacts;
      appState.currentFile = file;

      showAlert(`✅ ${result.message}`, 'success');
      displayFileInfo(file, result.stats);
      displayDataTable(result.data);
      displayStats(result.stats);
      generatePrologExamples(result.headers, result.data);
      
      // 🎯 MOSTRAR SECCIÓN 3D SOLO PARA DATOS
      show3DVisualizationSection();
      
      // Visualizar en 3D
      console.log('🚀 Datos cargados, visualizando en 3D...', result.data.length);
      
      setTimeout(() => {
        if (typeof visualizeDataIn3D === 'function') {
          visualizeDataIn3D(result.data, result.headers);
        } else {
          console.error('❌ Visualizador 3D no disponible');
          initialize3DVisualizer();
          setTimeout(() => visualizeDataIn3D(result.data, result.headers), 500);
        }
      }, 1000);
      
    } else {
      throw new Error(result.error);
    }
  } catch (error) {
    console.error('❌ Error procesando archivo:', error);
    showAlert(`❌ Error al procesar el archivo: ${error.message}`, 'danger');
  } finally {
    hideLoading();
  }
}

// 🔥 NUEVA FUNCIÓN: Mostrar sección 3D solo para datos
function show3DVisualizationSection() {
    const visualizationCard = document.getElementById('3dVisualizationCard');
    if (visualizationCard) {
        visualizationCard.style.display = 'block';
        
        // Scroll suave a la sección 3D
        setTimeout(() => {
            visualizationCard.scrollIntoView({ 
                behavior: 'smooth', 
                block: 'start' 
            });
        }, 500);
    }
}

// 🔥 NUEVA FUNCIÓN: Ocultar sección 3D para imágenes
function hide3DVisualizationSection() {
    const visualizationCard = document.getElementById('3dVisualizationCard');
    if (visualizationCard) {
        visualizationCard.style.display = 'none';
    }
}

// 🔥 FUNCIÓN PARA VISUALIZAR DATOS EN 3D
function visualizeDataIn3D(data, headers) {
    console.log('🎯 Iniciando visualización 3D con:', data.length, 'registros');
    
    const container = document.getElementById('3dContainer');
    const loadingElement = document.getElementById('loading3D');
    const noDataElement = document.getElementById('noData3D');
    const statsElement = document.getElementById('visualizationStats');
    
    if (!data || data.length === 0) {
        console.log('❌ No hay datos para visualizar');
        if (noDataElement) noDataElement.style.display = 'block';
        if (loadingElement) loadingElement.style.display = 'none';
        return;
    }
    
    // Ocultar "sin datos" y mostrar loading
    if (noDataElement) noDataElement.style.display = 'none';
    if (loadingElement) loadingElement.style.display = 'block';
    
    // Actualizar estadísticas
    updateVisualizationStats(data.length, '3D');
    
    // Esperar un momento para que se renderice el loading
    setTimeout(() => {
        try {
            // Llamar al visualizador 3D
            if (window.init3DVisualization) {
                window.init3DVisualization(data, headers);
            } else {
                console.error('❌ Visualizador 3D no disponible');
                showNotification('error', 'Error 3D', 'El visualizador 3D no está disponible');
            }
        } catch (error) {
            console.error('❌ Error en visualización 3D:', error);
            showNotification('error', 'Error 3D', `No se pudo generar la visualización: ${error.message}`);
        } finally {
            // Ocultar loading después de un tiempo
            setTimeout(() => {
                if (loadingElement) loadingElement.style.display = 'none';
            }, 2000);
        }
    }, 500);
}

// 🔥 ACTUALIZAR ESTADÍSTICAS DE VISUALIZACIÓN
function updateVisualizationStats(records, viewType) {
    const statRecords = document.getElementById('statRecords');
    const statObjects = document.getElementById('statObjects');
    const statView = document.getElementById('statView');
    
    if (statRecords) statRecords.textContent = records;
    if (statObjects) statObjects.textContent = records; // Cada registro es un objeto 3D
    if (statView) statView.textContent = viewType;
}

// 🔥 FUNCIONES DE CONTROL 3D
function toggle3DView() {
    const container = document.getElementById('3dContainer');
    if (container) {
        container.classList.toggle('fullscreen');
    }
}

function export3DScene() {
    showNotification('info', 'Exportar 3D', 'Funcionalidad de exportación en desarrollo');
}

function refresh3DVisualization() {
    const data = appState.currentData;
    const headers = data.length > 0 ? Object.keys(data[0]) : [];
    
    if (data.length > 0) {
        visualizeDataIn3D(data, headers);
        showNotification('info', 'Actualizando 3D', 'Regenerando visualización...');
    } else {
        showNotification('warning', 'Sin datos', 'No hay datos para visualizar');
    }
}

// Mejorar la función de cambio de tipo de visualización
function changeVisualizationType() {
    const type = document.getElementById('visualizationType').value;
    
    if (type === 'cube') {
        showNotification('info', 'Cubo Interactivo', 
            'Visualización en cubo 3D con navegación interactiva');
    }
    
    refresh3DVisualization();
}

function update3DLayout() {
    const axisX = document.getElementById('axisX').value;
    const axisY = document.getElementById('axisY').value;
    const axisZ = document.getElementById('axisZ').value;
    
    console.log('🔄 Actualizando layout 3D:', { axisX, axisY, axisZ });
    // Aquí actualizarías la visualización con los nuevos ejes
}

// Función para verificar que todos los elementos 3D estén disponibles
function check3DElements() {
    const requiredElements = [
        '3dContainer',
        '3dCanvas', 
        'noData3D',
        'loading3D',
        'visualizationStats'
    ];
    
    const missing = requiredElements.filter(id => !document.getElementById(id));
    
    if (missing.length > 0) {
        console.error('❌ Elementos 3D faltantes:', missing);
        return false;
    }
    
    console.log('✅ Todos los elementos 3D están presentes');
    return true;
}

// Agrega esta función para diagnosticar el problema
function diagnose3DProblem() {
    console.log('🔍 DIAGNÓSTICO 3D:');
    console.log('- appState.currentData:', appState.currentData?.length);
    console.log('- visualizer3D:', !!visualizer3D);
    console.log('- THREE:', typeof THREE);
    console.log('- Canvas:', document.getElementById('3dCanvas'));
    console.log('- visualizeDataIn3D function:', typeof visualizeDataIn3D);
    
    // Verificar datos específicos
    if (appState.currentData && appState.currentData.length > 0) {
        console.log('📊 Primer registro:', appState.currentData[0]);
        console.log('📊 Headers:', Object.keys(appState.currentData[0]));
    }
}

// Llamar esta función después de cargar datos
window.diagnose3D = diagnose3DProblem;

// function addForce3DButton() {
//     const forceBtn = document.createElement('button');
//     forceBtn.innerHTML = '🎯 Forzar 3D';
//     forceBtn.style.position = 'fixed';
//     forceBtn.style.top = '50px';
//     forceBtn.style.right = '10px';
//     forceBtn.style.zIndex = '10000';
//     forceBtn.style.background = '#ff6b6b';
//     forceBtn.style.color = 'white';
//     forceBtn.style.padding = '10px';
//     forceBtn.style.border = 'none';
//     forceBtn.style.borderRadius = '5px';
//     forceBtn.style.cursor = 'pointer';
    
//     forceBtn.onclick = function() {
//         console.log('🎯 Forzando visualización 3D...');
//         diagnose3DProblem();
        
//         if (appState.currentData && appState.currentData.length > 0) {
//             console.log('📊 Visualizando datos existentes...');
//             visualizeDataIn3D(appState.currentData);
//         } else {
//             console.log('❌ No hay datos en appState.currentData');
//             // Crear datos de prueba
//             const testData = [
//                 {x: 1, y: 2, z: 3, value: 10},
//                 {x: 2, y: 3, z: 4, value: 20},
//                 {x: 3, y: 4, z: 5, value: 30}
//             ];
//             appState.currentData = testData;
//             visualizeDataIn3D(testData);
//         }
//     };
    
//     document.body.appendChild(forceBtn);
// }

// Llamar en DOMContentLoaded
addForce3DButton();

// Función alternativa para cargar y visualizar datos
async function loadAndVisualizeData(file) {
    console.log('🎯 Cargando y visualizando datos...');
    
    const formData = new FormData();
    formData.append('file', file);
    formData.append('sessionId', appState.sessionId);

    try {
        const response = await fetch('/upload/data', {
            method: 'POST',
            body: formData
        });

        const result = await response.json();

        if (result.success) {
            // Guardar datos globalmente
            appState.currentData = result.data;
            
            console.log('✅ Datos cargados:', result.data.length, 'registros');
            
            // Inicializar visualizador si no existe
            if (!visualizer3D) {
                initialize3DVisualizer();
            }
            
            // Esperar y visualizar
            setTimeout(() => {
                if (visualizer3D && typeof visualizer3D.visualizeAllData === 'function') {
                    console.log('🎨 Llamando visualizeAllData...');
                    visualizer3D.visualizeAllData(result.data, result.headers);
                } else {
                    console.error('❌ Visualizador no disponible');
                }
            }, 1000);
            
            return result;
        }
    } catch (error) {
        console.error('❌ Error:', error);
    }
}

// Reemplazar temporalmente processDataFile
window.loadAndVisualizeData = loadAndVisualizeData;

let currentPrologFilter = '';
let filteredData = [];

async function applyPrologFilter() {
    const filterInput = document.getElementById('prologFilter');
    const filter = filterInput.value.trim();
    
    if (!filter) {
        showNotification('warning', 'Filtro vacío', 'Ingresa una consulta Prolog para filtrar');
        return;
    }
    
    if (!appState.currentData || appState.currentData.length === 0) {
        showNotification('warning', 'Sin datos', 'No hay datos cargados para filtrar');
        return;
    }
    
    showLoading('processing', 'Aplicando filtro Prolog...');
    
    try {
        // Ejecutar consulta de filtro
        const result = await executeSingleQueryHybrid(filter);
        
        if (result.success && result.results && result.results.length > 0) {
            currentPrologFilter = filter;
            filteredData = extractFilteredData(result.results, appState.currentData);
            
            // Actualizar visualización con datos filtrados
            visualizeDataIn3D(filteredData);
            
            // Actualizar estadísticas
            updateFilterStats(true, filteredData.length);
            
            showNotification('success', 'Filtro aplicado', 
                `${filteredData.length} registros coinciden con el filtro`);
                
        } else {
            showNotification('info', 'Sin resultados', 
                'El filtro no devolvió resultados. Mostrando todos los datos.');
            clearPrologFilter();
        }
        
    } catch (error) {
        showNotification('error', 'Error en filtro', 
            `No se pudo aplicar el filtro: ${error.message}`);
    } finally {
        hideLoading();
    }
}

function extractFilteredData(prologResults, originalData) {
    // Extraer IDs de los resultados de Prolog y filtrar datos originales
    const filteredIds = new Set();
    
    prologResults.forEach(result => {
        // Buscar ID en diferentes formatos posibles
        if (result.ID) filteredIds.add(parseInt(result.ID));
        if (result.Id) filteredIds.add(parseInt(result.Id));
        if (result.id) filteredIds.add(parseInt(result.id));
        if (result.X && !isNaN(parseInt(result.X))) filteredIds.add(parseInt(result.X));
    });
    
    return originalData.filter((_, index) => filteredIds.has(index + 1));
}

function clearPrologFilter() {
    currentPrologFilter = '';
    filteredData = [];
    
    document.getElementById('prologFilter').value = '';
    updateFilterStats(false, appState.currentData?.length || 0);
    
    // Restaurar visualización con todos los datos
    if (appState.currentData && appState.currentData.length > 0) {
        visualizeDataIn3D(appState.currentData);
    }
    
    showNotification('info', 'Filtro limpiado', 'Mostrando todos los datos');
}

function savePrologFilter() {
    const filter = document.getElementById('prologFilter').value.trim();
    
    if (!filter) {
        showNotification('warning', 'Filtro vacío', 'No hay filtro para guardar');
        return;
    }
    
    // Guardar en localStorage
    const savedFilters = JSON.parse(localStorage.getItem('saved3DFilters') || '[]');
    savedFilters.unshift({
        query: filter,
        timestamp: new Date().toISOString(),
        name: `Filtro_${new Date().getTime()}`
    });
    
    // Mantener solo los últimos 10 filtros
    if (savedFilters.length > 10) {
        savedFilters.pop();
    }
    
    localStorage.setItem('saved3DFilters', JSON.stringify(savedFilters));
    showNotification('success', 'Filtro guardado', 'El filtro se guardó para uso futuro');
}

function loadQuickFilter(type) {
    const quickFilters = {
        'numericos': "dato(ID, Columna, Valor), number(Valor), Valor > 0.",
        'textuales': "dato(ID, Columna, Valor), string(Valor), string_length(Valor, L), L > 0.",
        'unicos': "setof(Valor, Columna^dato(_, Columna, Valor), ValoresUnicos), member(Valor, ValoresUnicos), dato(ID, _, Valor)."
    };
    
    const filter = quickFilters[type];
    if (filter) {
        document.getElementById('prologFilter').value = filter;
        showNotification('info', 'Filtro cargado', 'Modifica el filtro si es necesario y aplícalo');
    }
}

function updateFilterStats(isActive, recordCount) {
    const statFilter = document.getElementById('statFilter');
    if (statFilter) {
        statFilter.textContent = isActive ? 'Sí' : 'No';
        statFilter.className = isActive ? 'stat-value active' : 'stat-value';
    }
    
    const statRecords = document.getElementById('statRecords');
    if (statRecords) {
        statRecords.textContent = recordCount;
    }
}

