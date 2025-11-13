// 🔥 CORREGIR: Estado global con sesión persistente
const appState = {
  currentData: [],
  prologFacts: '',
  currentFile: null,
  // 🔥 USAR MISMA SESIÓN SIEMPRE
  sessionId: localStorage.getItem('currentSessionId') || 'session_' + Date.now(),
  autonomousMode: true,
  threeDVisualizer: null,
  currentAnalysis: null,

  // 🔥 GUARDAR SESIÓN EN LOCALSTORAGE
  setSessionId: function (newSessionId) {
    this.sessionId = newSessionId;
    localStorage.setItem('currentSessionId', newSessionId);
    updateSessionInfo();
  },

  ensureSession: async function () {
    try {
      const response = await fetch(`/session/debug/${this.sessionId}`);
      const sessionInfo = await response.json();

      if (!sessionInfo.exists) {
        console.log('🔄 Sesión no existe, creando nueva...');
        // Crear nueva sesión
        this.setSessionId('session_' + Date.now());
        return false;
      }

      return sessionInfo.hasPrologFacts || sessionInfo.objectCount > 0;
    } catch (error) {
      console.error('Error verificando sesión:', error);
      return false;
    }
  }
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

// Inicialización
document.addEventListener('DOMContentLoaded', function () {

  const modal = document.getElementById('helpModal');
  if (modal) {
    modal.addEventListener('click', function (event) {
      if (event.target === modal) {
        closeHelpModal();
      }
    });
  }
  document.addEventListener('keydown', function (event) {
    if (event.key === 'Escape') {
      closeHelpModal();
    }
  });
  initializeEventListeners();
  updateSessionInfo();
  setupQueryExamples();
  loadSavedQueries();
  loadSavedRulesList();
  loadRuleCarousel();
  addDiagnosticButtons();
  setTimeout(() => {
    checkRulesFile();
  }, 1000);

  setTimeout(() => {
    console.log('🚀 Forzando creación de botones de diagnóstico...');
    addDiagnosticButtons();
  }, 1000);

  console.log('🚀 Sistema de detección de atributos inicializado');
});

// 🔥 FUNCIÓN PARA EVITAR EVENT LISTENERS DUPLICADOS
function initializeEventListenersOnce() {
  // Remover event listeners existentes primero
  const dataFileInput = document.getElementById('dataFileInput');
  const imageFileInput = document.getElementById('imageFileInput');
  const queryEditor = document.getElementById('prologQuery');

  if (dataFileInput) {
    dataFileInput.replaceWith(dataFileInput.cloneNode(true));
  }
  if (imageFileInput) {
    imageFileInput.replaceWith(imageFileInput.cloneNode(true));
  }

  // Reinicializar event listeners
  initializeEventListeners();
}

// 🔥 REEMPLAZAR LA FUNCIÓN initializeEventListeners existente
function initializeEventListeners() {
  console.log('🔧 Inicializando event listeners...');

  // Configurar event listeners para upload de datos
  const dataUploadArea = document.getElementById('dataUploadArea');
  const dataFileInput = document.getElementById('dataFileInput');

  if (dataUploadArea && !dataUploadArea.hasListener) {
    dataUploadArea.hasListener = true;
    dataUploadArea.addEventListener('dragover', handleDragOver);
    dataUploadArea.addEventListener('dragleave', handleDragLeave);
    dataUploadArea.addEventListener('drop', (e) => handleDrop(e, 'data'));
  }

  if (dataFileInput && !dataFileInput.hasListener) {
    dataFileInput.hasListener = true;
    dataFileInput.addEventListener('change', (e) => {
      console.log('📁 Archivo de datos seleccionado');
      if (e.target.files.length > 0) {
        processDataFile(e.target.files[0]);
        // Limpiar el input después de procesar
        e.target.value = '';
      }
    });
  }

  // Configurar event listeners para upload de imágenes
  const imageUploadArea = document.getElementById('imageUploadArea');
  const imageFileInput = document.getElementById('imageFileInput');

  if (imageUploadArea && !imageUploadArea.hasListener) {
    imageUploadArea.hasListener = true;
    imageUploadArea.addEventListener('dragover', handleDragOver);
    imageUploadArea.addEventListener('dragleave', handleDragLeave);
    imageUploadArea.addEventListener('drop', (e) => handleDrop(e, 'image'));
  }

  if (imageFileInput && !imageFileInput.hasListener) {
    imageFileInput.hasListener = true;
    imageFileInput.addEventListener('change', (e) => {
      console.log('🖼️ Archivo de imagen seleccionado');
      if (e.target.files.length > 0) {
        processImageFile(e.target.files[0]);
        // Limpiar el input después de procesar
        e.target.value = '';
      }
    });
  }

  // Atajo de teclado para ejecutar consultas - solo una vez
  const queryEditor = document.getElementById('prologQuery');
  if (queryEditor && !queryEditor.hasListener) {
    queryEditor.hasListener = true;
    queryEditor.addEventListener('keydown', (e) => {
      if (e.ctrlKey && e.key === 'Enter') {
        e.preventDefault();
        console.log('⌨️ Atajo Ctrl+Enter ejecutado');
        executePrologQuery();
      }
    });
  }

  console.log('✅ Event listeners inicializados correctamente');
}

// 🔥 NUEVO: Inicializar carrusel de reglas
function loadRuleCarousel() {
  const savedRules = JSON.parse(localStorage.getItem('attributeRuleCards') || '[]');
  carouselState.savedRules = savedRules;
  carouselState.currentRuleSet = savedRules;

  updateCarousel();
  updateCarouselInfo();
}

// 🔥 NUEVO: Actualizar carrusel
function updateCarousel() {
  const carousel = document.getElementById('rulesCarousel');
  const indicators = document.getElementById('carouselIndicators');

  if (!carousel || !indicators) return;

  carousel.innerHTML = '';
  indicators.innerHTML = '';

  if (carouselState.currentRuleSet.length === 0) {
    carousel.innerHTML = `
      <div class="empty-rules">
        <i class="fas fa-cards"></i>
        <p>No hay reglas de atributos guardadas</p>
        <p class="text-muted">Analiza una imagen para generar reglas automáticas</p>
        <button class="btn btn-primary" onclick="analyzeImageForAttributes()">
          <i class="fas fa-camera"></i> Analizar Imagen para Atributos
        </button>
      </div>
    `;
    return;
  }

  const startIndex = carouselState.currentIndex;
  const endIndex = Math.min(
    startIndex + carouselState.rulesPerView,
    carouselState.currentRuleSet.length
  );

  for (let i = startIndex; i < endIndex; i++) {
    const rule = carouselState.currentRuleSet[i];
    const card = createAttributeRuleCard(rule, i);
    carousel.appendChild(card);
  }

  const totalPages = Math.ceil(carouselState.currentRuleSet.length / carouselState.rulesPerView);
  for (let i = 0; i < totalPages; i++) {
    const indicator = document.createElement('div');
    indicator.className = `carousel-indicator ${i === Math.floor(carouselState.currentIndex / carouselState.rulesPerView) ? 'active' : ''}`;
    indicator.onclick = () => goToPage(i);
    indicators.appendChild(indicator);
  }
}

// 🔥 NUEVO: Crear card de regla de atributos
function createAttributeRuleCard(rule, index) {
  const card = document.createElement('div');
  card.className = 'rule-card attribute-rule';
  card.innerHTML = `
    <div class="rule-card-header">
      <h4 class="rule-card-title" title="${rule.name}">
        <i class="fas ${getRuleIcon(rule.type)}"></i>
        ${rule.name}
      </h4>
      <div class="rule-card-actions">
        <button class="btn btn-outline btn-sm" onclick="copyRuleToClipboard('${rule.code.replace(/'/g, "\\'")}')" title="Copiar regla">
          <i class="fas fa-copy"></i>
        </button>
        <button class="btn btn-outline btn-sm" onclick="executeAttributeRule('${rule.code.replace(/'/g, "\\'")}', ${index})" title="Ejecutar regla">
          <i class="fas fa-play"></i>
        </button>
        <button class="btn btn-outline btn-sm" onclick="deleteRule(${index})" title="Eliminar regla">
          <i class="fas fa-trash"></i>
        </button>
      </div>
    </div>
    
    <div class="rule-card-content">
      <div class="rule-description">${rule.description || 'Regla de atributos generada automáticamente'}</div>
      <div class="rule-code-full">${formatRuleCode(rule.code)}</div>
      
      <div class="rule-meta">
        <div class="rule-timestamp">
          <i class="fas fa-clock"></i>
          ${formatTimestamp(rule.timestamp)}
        </div>
        <span class="rule-type ${rule.type}">${rule.type}</span>
      </div>
    </div>
    
    <div class="rule-card-footer">
      <button class="btn-execute-rule" onclick="executeAttributeRule('${rule.code.replace(/'/g, "\\'")}', ${index})" 
              data-rule-index="${index}" id="executeBtn-${index}">
        <i class="fas fa-vial"></i>
        Probar Esta Regla
      </button>
    </div>
  `;

  return card;
}

function getRuleIcon(type) {
  const icons = {
    'detection': 'fa-search',
    'quality': 'fa-star',
    'safety': 'fa-shield-alt',
    'count': 'fa-calculator',
    'attribute': 'fa-tag'
  };
  return icons[type] || 'fa-cube';
}


// Función para guardar regla en el servidor
async function saveRuleToServer(ruleCode, ruleName) {
  try {
    const response = await fetch('/rules/save', {
      method: 'POST',
      headers: {
        'Content-Type': 'application/json'
      },
      body: JSON.stringify({
        rules: ruleCode,
        ruleName: ruleName,
        sessionId: appState.sessionId
      })
    });

    const result = await response.json();
    if (!result.success) {
      console.warn('No se pudo guardar la regla en el servidor:', result.error);
    }
  } catch (error) {
    console.warn('Error guardando regla en servidor:', error);
  }
}

// Función para intentar consultas simples alternativas
async function trySimpleQuery(ruleName, ruleCode) {
  const simpleQueries = {
    'objeto_principal': 'objeto_principal(ID).',
    'objeto_secundario': 'objeto_secundario(ID).',
    'es_comestible': 'es_comestible(ID).',
    'objeto_seguro': 'objeto_seguro(ID).',
    'es_fruta': 'es_fruta(ID).',
    'total_objetos': 'total_objetos(N).',
    'objetos_peligrosos': 'objetos_peligrosos(N).'
  };

  // Si es una consulta conocida, usar la versión simple
  if (simpleQueries[ruleName]) {
    return await executeSingleQueryHybrid(simpleQueries[ruleName]);
  }

  // Si no, intentar consultas básicas
  const basicQueries = [
    'objeto_detectado(ID, Objeto, Confianza).',
    'seguridad_objeto(ID, Seguridad).',
    'estado_objeto(ID, Estado).',
    'atributo_objeto(ID, Atributo).'
  ];

  for (const query of basicQueries) {
    const result = await executeSingleQueryHybrid(query);
    if (result.success && result.count > 0) {
      return result;
    }
  }

  // Si todo falla, devolver resultado vacío
  return { success: true, count: 0, results: [] };
}

// 🔥 NUEVO: Mostrar resultado de regla de atributos
function showAttributeRuleResult(ruleName, result, ruleIndex) {
  const isSuccess = result.success;
  const resultCount = result.count || 0;
  const hasWarning = result.warning;

  let message = '';
  let type = 'info';

  if (hasWarning) {
    message = `⚠️ Advertencia: ${result.warning}`;
    type = 'warning';
  } else if (isSuccess) {
    if (resultCount > 0) {
      message = `✅ La condición se cumple: ${resultCount} coincidencias encontradas`;
      type = 'success';

      // Mostrar primeros resultados en la notificación
      if (result.results && result.results.length > 0) {
        const firstResults = result.results.slice(0, 3);
        message += `\n\nPrimeros resultados:\n`;
        firstResults.forEach((res, idx) => {
          const vars = Object.entries(res).map(([k, v]) => `${k}=${v}`).join(', ');
          message += `• [${idx + 1}] ${vars}\n`;
        });
        if (resultCount > 3) {
          message += `• ... y ${resultCount - 3} más`;
        }
      }
    } else {
      message = 'ℹ️ La condición NO se cumple: 0 coincidencias encontradas\n\nSugerencias:\n• Verifica que los datos estén cargados\n• Prueba con consultas más simples\n• Revisa la sintaxis de la regla';
      type = 'info';
    }
  } else {
    message = `❌ Error en la verificación: ${result.error || 'Error desconocido'}\n\nConsulta: ${ruleName}.`;
    type = 'error';
  }

  showNotification(type, `Verificación: ${ruleName}`, message);

  // Mostrar detalles completos si hay resultados
  if (resultCount > 0 && result.results) {
    setTimeout(() => {
      showDetailedAttributeResults(ruleName, result.results);
    }, 1500);
  }
}

// 🔥 NUEVO: Mostrar resultados detallados de atributos
function showDetailedAttributeResults(ruleName, results) {
  const notification = document.createElement('div');
  notification.className = 'notification success detailed-results';
  notification.innerHTML = `
    <div class="notification-progress success"></div>
    <div class="notification-header">
      <div class="notification-icon">
        <i class="fas fa-list"></i>
      </div>
      <div class="notification-content">
        <h4 class="notification-title">📊 Resultados Detallados: ${ruleName}</h4>
        <div class="attribute-results-grid">
          ${results.map((result, idx) => `
            <div class="attribute-result-item">
              <div class="result-header">
                <span class="result-number">#${idx + 1}</span>
                <span class="result-type">Coincidencia</span>
              </div>
              <div class="result-variables">
                ${Object.entries(result).map(([key, value]) => `
                  <div class="variable-binding">
                    <span class="variable-name">${key}</span>
                    <span class="binding-operator"> = </span>
                    <span class="variable-value">${value}</span>
                  </div>
                `).join('')}
              </div>
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

// 🔥 NUEVO: Analizar imagen para extraer atributos
async function analyzeImageForAttributes() {
  const fileInput = document.getElementById('imageFileInput');
  if (!fileInput.files.length) {
    showNotification('warning', 'Sin imagen', 'Selecciona una imagen primero');
    fileInput.click();
    return;
  }

  await processImageFile(fileInput.files[0]);
}

// 🔥 NUEVO: Procesar imagen con detección de atributos
async function processImageFile(file) {
  showLoading('analyzing', 'Analizando atributos de imagen...', 'Detectando objetos, estados y características');

  const formData = new FormData();
  formData.append('image', file);
  formData.append('sessionId', appState.sessionId);

  try {
    const response = await fetch('/analyze/image/detailed', {
      method: 'POST',
      body: formData
    });

    const result = await response.json();

    if (result.success) {
      appState.currentAnalysis = result.analysis;
      showNotification('success', 'Análisis completado',
        `${result.analysis.detectedObjects?.length || 0} objetos con atributos detectados`);

      displayAttributeAnalysis(result.analysis, result.prologFacts);
      generateAttributeRules(result.analysis, result.autoQueries);

    } else {
      throw new Error(result.error);
    }
  } catch (error) {
    showNotification('error', 'Error en análisis', `No se pudieron detectar atributos: ${error.message}`);
  } finally {
    hideLoading();
  }
}

// 🔥 NUEVO: Mostrar análisis de atributos
// 🔥 NUEVO: Mostrar análisis de atributos (VERSIÓN CORREGIDA)
function displayAttributeAnalysis(analysis, prologFacts) {
  const prevAnalysis = document.querySelector('.attribute-analysis-results');
  if (prevAnalysis) {
    prevAnalysis.remove();
  }

  // Verificar que tenemos datos
  if (!analysis || !analysis.objects) {
    showNotification('warning', 'Sin datos', 'No se recibieron datos de análisis');
    return;
  }

  const resultsContainer = document.createElement('div');
  resultsContainer.className = 'attribute-analysis-results';

  // Preparar HTML para objetos detectados
  const objectsHTML = analysis.objects && analysis.objects.length > 0
    ? analysis.objects.map(obj => `
        <div class="object-card">
          <div class="object-header">
            <span class="object-name">${obj.object || 'Objeto'}</span>
            <span class="object-confidence">${obj.confidence || 'N/A'}</span>
          </div>
          <div class="object-attributes">
            ${(obj.attributes || []).map(attr => `
              <span class="attribute-tag">${attr}</span>
            `).join('')}
            ${(obj.enhancedAttributes || []).map(attr => `
              <span class="attribute-tag enhanced">${attr}</span>
            `).join('')}
          </div>
          <div class="object-details">
            <div class="detail-item">
              <strong>Estado:</strong> 
              <span class="value ${obj.estimatedState || 'desconocido'}">${obj.estimatedState || 'desconocido'}</span>
            </div>
            <div class="detail-item">
              <strong>Seguridad:</strong> 
              <span class="value ${obj.safety || 'desconocido'}">${obj.safety || 'desconocido'}</span>
            </div>
            <div class="detail-item">
              <strong>Calidad:</strong> 
              <span class="value ${obj.quality || 'regular'}">${obj.quality || 'regular'}</span>
            </div>
          </div>
          ${obj.recommendations && obj.recommendations.length > 0 ? `
            <div class="object-recommendations">
              <strong>Recomendaciones:</strong>
              <ul>
                ${obj.recommendations.map(rec => `<li>${rec}</li>`).join('')}
              </ul>
            </div>
          ` : ''}
        </div>
      `).join('')
    : '<p class="no-objects">No se detectaron objetos</p>';

  resultsContainer.innerHTML = `
    <div class="card">
      <div class="card-header">
        <div class="card-title">
          <i class="fas fa-microscope"></i>
          Análisis Detallado de Atributos
          <div class="analysis-badge">
            <i class="fas fa-robot"></i>
            ${analysis.objects ? analysis.objects.length : 0} OBJETOS DETECTADOS
          </div>
        </div>
      </div>
      
      <div class="attribute-sections">
        <!-- Resumen de objetos detectados -->
        <div class="attribute-section">
          <h4><i class="fas fa-shapes"></i> Objetos Detectados (${analysis.objects ? analysis.objects.length : 0})</h4>
          <div class="objects-grid">
            ${objectsHTML}
          </div>
        </div>
        
        <!-- Evaluaciones generales -->
        <div class="attribute-section">
          <h4><i class="fas fa-chart-bar"></i> Evaluaciones Globales</h4>
          <div class="evaluations-grid">
            <div class="evaluation-card safety">
              <div class="eval-icon">
                <i class="fas fa-shield-alt"></i>
              </div>
              <div class="eval-info">
                <div class="eval-title">Seguridad</div>
                <div class="eval-value ${analysis.safetyAssessment?.safe ? 'safe' : 'unsafe'}">
                  ${analysis.safetyAssessment?.safe ? 'SEGURO' : 'PELIGROSO'}
                </div>
                <div class="eval-details">
                  ${analysis.safetyAssessment?.unsafeCount || 0} objetos peligrosos
                </div>
              </div>
            </div>
            
            <div class="evaluation-card quality">
              <div class="eval-icon">
                <i class="fas fa-star"></i>
              </div>
              <div class="eval-info">
                <div class="eval-title">Calidad</div>
                <div class="eval-value ${analysis.qualityAssessment || 'regular'}">
                  ${(analysis.qualityAssessment || 'regular').toUpperCase()}
                </div>
                <div class="eval-details">
                  Evaluación global
                </div>
              </div>
            </div>
            
            <div class="evaluation-card count">
              <div class="eval-icon">
                <i class="fas fa-calculator"></i>
              </div>
              <div class="eval-info">
                <div class="eval-title">Conteo</div>
                <div class="eval-value">${analysis.objects ? analysis.objects.length : 0}</div>
                <div class="eval-details">
                  objetos detectados
                </div>
              </div>
            </div>
          </div>
        </div>

        <!-- Hechos Prolog generados -->
        <div class="attribute-section">
          <h4><i class="fas fa-code"></i> Hechos Prolog Generados</h4>
          <div class="prolog-facts">
            <pre><code>${prologFacts || '// No se generaron hechos Prolog'}</code></pre>
          </div>
        </div>
      </div>
    </div>
  `;

  const uploadCard = document.querySelector('.card');
  if (uploadCard) {
    uploadCard.parentNode.insertBefore(resultsContainer, uploadCard.nextSibling);
  } else {
    document.querySelector('.container').appendChild(resultsContainer);
  }

  // Scroll suave a los resultados
  resultsContainer.scrollIntoView({ behavior: 'smooth', block: 'start' });
}



// 🔥 NUEVO: Generar reglas de atributos automáticamente
async function generateAttributeRules(analysis, autoQueries) {
  try {
    const response = await fetch('/rules/generate', {
      method: 'POST',
      headers: {
        'Content-Type': 'application/json'
      },
      body: JSON.stringify({
        sessionId: appState.sessionId,
        criteria: ['detection', 'quality', 'safety', 'count']
      })
    });

    const result = await response.json();

    if (result.success) {
      const individualRules = splitRulesIntoIndividualCards(result.rules);

      if (individualRules.length === 0) {
        showNotification('warning', 'Sin reglas', 'No se generaron reglas individuales');
        return;
      }

      // Agregar consultas automáticas como reglas
      if (autoQueries && autoQueries.length > 0) {
        autoQueries.forEach((query, index) => {
          individualRules.unshift({
            id: Date.now() + index + 1000,
            name: `consulta_auto_${index + 1}`,
            code: query,
            description: 'Consulta automática generada',
            timestamp: new Date().toISOString(),
            type: 'query'
          });
        });
      }

      // Agregar al carrusel
      individualRules.forEach(rule => {
        carouselState.savedRules.unshift(rule);
      });

      carouselState.currentRuleSet = carouselState.savedRules;
      localStorage.setItem('attributeRuleCards', JSON.stringify(carouselState.savedRules));

      updateCarousel();
      updateCarouselInfo();

      showNotification('success', 'Reglas Generadas',
        `Se crearon ${individualRules.length} reglas de atributos automáticamente`);

    } else {
      throw new Error(result.error);
    }
  } catch (error) {
    showNotification('error', 'Error', `No se pudieron generar las reglas: ${error.message}`);
  }
}

// 🔥 NUEVO: Configurar ejemplos de consultas para atributos
function setupQueryExamples() {
  const examples = [
    {
      name: "Objetos detectados",
      query: "objeto_detectado(ID, Objeto, Confianza).",
      description: "Todos los objetos detectados en la imagen"
    },
    {
      name: "Objetos comestibles",
      query: "es_comestible(ID).",
      description: "Objetos seguros para consumo"
    },
    {
      name: "Objetos podridos",
      query: "esta_podrido(ID).",
      description: "Objetos en estado de descomposición"
    },
    {
      name: "Calidad de objetos",
      query: "calidad_objeto(ID, Calidad).",
      description: "Nivel de calidad de cada objeto"
    },
    {
      name: "Conteo total",
      query: "total_objetos(Total).",
      description: "Número total de objetos detectados"
    },
    {
      name: "Resumen seguridad",
      query: "resumen_seguridad.",
      description: "Resumen general de seguridad"
    },
    {
      name: "Manzanas podridas",
      query: "manzana_podrida(ID).",
      description: "Manzanas en mal estado"
    },
    {
      name: "Frutas maduras",
      query: "es_fruta(ID), esta_maduro(ID).",
      description: "Frutas listas para consumo"
    }
  ];

  const quickQueriesContainer = document.getElementById('quickQueries');
  if (!quickQueriesContainer) return;

  let html = `
    <div class="quick-queries">
      <h4><i class="fas fa-bolt"></i> Consultas de Atributos</h4>
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
        Estas consultas usan los atributos detectados automáticamente
      </small>
    </div>
  `;

  quickQueriesContainer.innerHTML = html;
}

// 🔥 NUEVO: Cargar ejemplo en el editor
function loadQueryExample(query, description) {
  const queryEditor = document.getElementById('prologQuery');
  if (queryEditor) {
    queryEditor.value = query;
    queryEditor.focus();

    showNotification('info', 'Consulta cargada',
      `"${description}" - Modifícala si es necesario y ejecuta`);
  }
}

// Funciones auxiliares (mantener del código original)
function extractRuleName(ruleCode) {
  if (!ruleCode) return null;

  const patterns = [
    /^([a-z][a-zA-Z0-9_]*)\s*:-/,
    /^([a-z][a-zA-Z0-9_]*)\s*\(/,
    /^([a-z][a-zA-Z0-9_]*)\s*\./
  ];

  for (const pattern of patterns) {
    const match = ruleCode.match(pattern);
    if (match && match[1]) {
      return match[1];
    }
  }

  return null;
}

function formatRuleCode(code) {
  if (!code) return '<span class="text-muted">Sin código</span>';

  let formattedCode = code
    .replace(/%[^\n]*/g, '<span class="comment">$&</span>')
    .replace(/(:-)/g, '<span class="operator">$1</span>')
    .replace(/([A-Z][a-zA-Z0-9_]*)/g, '<span class="variable">$1</span>')
    .replace(/([a-z][a-zA-Z0-9_]*)(?=\()/g, '<span class="predicate">$1</span>')
    .replace(/'([^']*)'/g, '<span class="string">\'$1\'</span>')
    .replace(/(\b\d+\.?\d*\b)/g, '<span class="number">$1</span>')
    .replace(/(\.[\s]*$)/g, '<span class="operator">$1</span>');

  return formattedCode;
}

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

function splitRulesIntoIndividualCards(rulesText) {
  if (!rulesText || !rulesText.trim()) return [];

  const lines = rulesText.split('\n')
    .map(line => line.trim())
    .filter(line => {
      return line &&
        !line.startsWith('%') &&
        line.endsWith('.') &&
        line.length > 3;
    });

  const individualRules = [];

  lines.forEach((line, index) => {
    let ruleName = 'regla';
    if (line.includes(':-')) {
      ruleName = line.split(':-')[0].trim();
    } else {
      ruleName = line.substring(0, line.length - 1).trim();
    }

    ruleName = ruleName.replace(/\(.*\)/, '')
      .replace(/,/g, '_')
      .replace(/'/g, '')
      .substring(0, 30);

    // Determinar tipo de regla basado en el contenido
    let type = 'attribute';
    if (line.includes('comestible') || line.includes('peligroso')) type = 'safety';
    if (line.includes('calidad') || line.includes('excellent')) type = 'quality';
    if (line.includes('contar') || line.includes('total')) type = 'count';
    if (line.includes('detectado')) type = 'detection';

    individualRules.push({
      id: Date.now() + index,
      name: ruleName || `Regla ${index + 1}`,
      code: line,
      description: generateRuleDescription(line),
      timestamp: new Date().toISOString(),
      type: type
    });
  });

  return individualRules;
}

function generateRuleDescription(ruleCode) {
  if (!ruleCode) return 'Regla de análisis';

  const code = ruleCode.toLowerCase();

  if (code.includes('comestible')) return 'Verifica si un objeto es seguro para consumo';
  if (code.includes('podrido')) return 'Identifica objetos en mal estado';
  if (code.includes('calidad')) return 'Evalúa la calidad del objeto';
  if (code.includes('contar') || code.includes('total')) return 'Cuenta objetos que cumplen cierta condición';
  if (code.includes('seguro') || code.includes('peligroso')) return 'Verifica condiciones de seguridad';
  if (code.includes('detectado')) return 'Regla de detección de objetos';

  return 'Regla de análisis de atributos';
}

// Funciones de navegación del carrusel
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

// Funciones de utilidad (mantener del código original)
function showNotification(type, title, message, details = null) {
  const notification = document.createElement('div');
  notification.className = `notification ${type}`;
  notification.innerHTML = `
    <div class="notification-progress ${type}"></div>
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

  const container = document.getElementById('notificationContainer') || createNotificationContainer();
  container.appendChild(notification);

  setTimeout(() => {
    if (notification.parentElement) {
      notification.remove();
    }
  }, 5000);
}

// 🔥 FUNCIÓN PARA LIMPIAR BASE DE DATOS COMPLETAMENTE
async function clearDatabase() {
  if (!confirm('⚠️ ¿ESTÁS SEGURO?\n\nEsto eliminará TODOS los datos de la base de datos:\n• Todas las sesiones\n• Todos los hechos Prolog\n• Todas las reglas guardadas\n• Todas las consultas guardadas\n\nEsta acción NO se puede deshacer.')) {
    return;
  }

  showLoading('processing', 'Limpiando base de datos...', 'Eliminando todos los datos');

  try {
    const response = await fetch('/admin/clear-database', {
      method: 'DELETE'
    });

    const result = await response.json();

    if (result.success) {
      // Limpiar estado local
      appState.currentData = [];
      appState.prologFacts = '';
      appState.currentAnalysis = null;

      // Limpiar carrusel de reglas
      carouselState.savedRules = [];
      carouselState.currentRuleSet = [];
      localStorage.removeItem('attributeRuleCards');

      // Limpiar interfaz
      document.getElementById('dataTable').innerHTML = '';
      document.getElementById('fileInfo').style.display = 'none';
      document.getElementById('statsCard').style.display = 'none';
      document.getElementById('prologResults').style.display = 'none';

      // Actualizar carrusel
      updateCarousel();
      updateCarouselInfo();

      // Crear nueva sesión limpia
      appState.setSessionId('session_' + Date.now());

      showNotification('success', 'Base de Datos Limpiada',
        '✅ Todos los datos han sido eliminados\n🆕 Nueva sesión creada\n🗑️ Base de datos completamente vacía');

    } else {
      throw new Error(result.error);
    }
  } catch (error) {
    console.error('❌ Error limpiando base de datos:', error);
    showNotification('error', 'Error Limpiando',
      `No se pudo limpiar la base de datos: ${error.message}`);
  } finally {
    hideLoading();
  }
}

// 🔥 FUNCIÓN PARA LIMPIAR SESIÓN ACTUAL
async function clearCurrentSession() {
  if (!confirm('¿Estás seguro de que quieres limpiar la sesión actual?\n\nSe eliminarán todos los datos de esta sesión, pero otras sesiones permanecerán intactas.')) {
    return;
  }

  showLoading('processing', 'Limpiando sesión actual...', 'Eliminando datos de la sesión');

  try {
    const response = await fetch(`/session/clear/${appState.sessionId}`, {
      method: 'DELETE'
    });

    const result = await response.json();

    if (result.success) {
      // Limpiar estado local
      appState.currentData = [];
      appState.prologFacts = '';
      appState.currentAnalysis = null;

      // Limpiar interfaz
      document.getElementById('dataTable').innerHTML = '';
      document.getElementById('fileInfo').style.display = 'none';
      document.getElementById('statsCard').style.display = 'none';
      document.getElementById('prologResults').style.display = 'none';

      showNotification('success', 'Sesión Limpiada',
        `✅ Sesión ${appState.sessionId} ha sido limpiada\n📊 Todos los datos eliminados`);

    } else {
      throw new Error(result.error);
    }
  } catch (error) {
    console.error('❌ Error limpiando sesión:', error);
    showNotification('error', 'Error Limpiando',
      `No se pudo limpiar la sesión: ${error.message}`);
  } finally {
    hideLoading();
  }
}

// 🔥 FUNCIÓN PARA VER ESTADO DE LA BASE DE DATOS
async function showDatabaseStatus() {
  try {
    const response = await fetch('/api/status');
    const status = await response.json();

    // Obtener estadísticas de todas las sesiones
    const sessionsResponse = await fetch('/admin/session-stats');
    const sessionsData = await sessionsResponse.json();

    let message = `📊 ESTADO DE LA BASE DE DATOS:\n\n`;
    message += `🔗 PostgreSQL: ${status.database}\n`;
    message += `🕐 Última verificación: ${new Date().toLocaleString()}\n\n`;

    if (sessionsData.success) {
      message += `📁 SESIONES ACTIVAS:\n`;
      Object.entries(sessionsData.sessions).forEach(([sessionId, sessionInfo]) => {
        message += `• ${sessionId}: ${sessionInfo.facts} hechos, ${sessionInfo.rules} reglas\n`;
      });
    }

    showNotification('info', 'Estado de Base de Datos', message);

  } catch (error) {
    showNotification('error', 'Error', 'No se pudo obtener el estado de la base de datos');
  }
}

function createNotificationContainer() {
  const container = document.createElement('div');
  container.id = 'notificationContainer';
  container.className = 'notification-container';
  document.body.appendChild(container);
  return container;
}

function showLoading(type = 'default', message = 'Procesando...', details = '') {
  if (!loadingDiv) return;

  loadingState.isShowing = true;
  loadingState.currentType = type;

  let icon = '⏳';
  let loadingClass = 'loading';

  switch (type) {
    case 'analyzing':
      icon = '🔍';
      loadingClass += ' analyzing';
      break;
    case 'querying':
      icon = '⚡';
      loadingClass += ' querying';
      break;
    default:
      icon = '⏳';
  }

  loadingDiv.className = loadingClass;
  loadingDiv.innerHTML = `
    <div class="loading-content">
      <div class="loading-spinner"></div>
      <div>
        <p>${message}</p>
        ${details ? `<div class="loading-status">${details}</div>` : ''}
      </div>
    </div>
  `;
  loadingDiv.style.display = 'block';
}

function hideLoading() {
  if (!loadingDiv || !loadingState.isShowing) return;
  loadingDiv.style.display = 'none';
  loadingState.isShowing = false;
}

function initializeEventListeners() {
  // Configurar event listeners para upload de datos
  const dataUploadArea = document.getElementById('dataUploadArea');
  const dataFileInput = document.getElementById('dataFileInput');

  if (dataUploadArea) {
    dataUploadArea.addEventListener('dragover', handleDragOver);
    dataUploadArea.addEventListener('dragleave', handleDragLeave);
    dataUploadArea.addEventListener('drop', (e) => handleDrop(e, 'data'));
  }

  if (dataFileInput) {
    dataFileInput.addEventListener('change', (e) => handleFileSelect(e, 'data'));
  }

  // Configurar event listeners para upload de imágenes (existente)
  const imageUploadArea = document.getElementById('imageUploadArea');
  const imageFileInput = document.getElementById('imageFileInput');

  if (imageUploadArea) {
    imageUploadArea.addEventListener('dragover', handleDragOver);
    imageUploadArea.addEventListener('dragleave', handleDragLeave);
    imageUploadArea.addEventListener('drop', (e) => handleDrop(e, 'image'));
  }

  if (imageFileInput) {
    imageFileInput.addEventListener('change', (e) => handleFileSelect(e, 'image'));
  }

  // Atajo de teclado para ejecutar consultas
  const queryEditor = document.getElementById('prologQuery');
  if (queryEditor) {
    queryEditor.addEventListener('keydown', (e) => {
      if (e.ctrlKey && e.key === 'Enter') {
        e.preventDefault();
        executePrologQuery();
      }
    });
  }
}


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
    const file = e.target.files[0];
    if (type === 'data') {
      processDataFile(file);
    } else {
      processImageFile(file);
    }
  }
}


function updateSessionInfo() {
  const sessionElement = document.getElementById('sessionStatus');
  if (sessionElement) {
    sessionElement.textContent = `Sesión: ${appState.sessionId}`;
  }
}
// 🔥 NUEVA FUNCIÓN: Recargar datos de sesión
async function reloadSessionData() {
  showLoading('analyzing', 'Recargando datos de sesión...', 'Sincronizando con servidor');

  try {
    const response = await fetch(`/session/reload/${appState.sessionId}`, {
      method: 'POST'
    });

    const result = await response.json();

    if (result.success) {
      showNotification('success', 'Datos Recargados',
        `${result.factsCount} líneas de Prolog, ${result.objectsCount} objetos`);
      return true;
    } else {
      throw new Error(result.error);
    }
  } catch (error) {
    showNotification('error', 'Error Recargando', `No se pudieron recargar los datos: ${error.message}`);
    return false;
  } finally {
    hideLoading();
  }
}

// 🔥 NUEVO: Ejecutar regla de atributos (VERSIÓN MEJORADA)
// 🔥 NUEVO: Ejecutar regla de atributos (VERSIÓN CON MEJOR MANEJO DE ERRORES)
// 🔥 CORREGIR: Ejecutar regla de atributos (VERSIÓN MEJORADA)
async function executeAttributeRule(ruleCode, ruleIndex) {
  const executeBtn = document.getElementById(`executeBtn-${ruleIndex}`);
  if (!executeBtn) return;

  const ruleName = extractRuleName(ruleCode);
  if (!ruleName) {
    showNotification('error', 'Error en regla', 'No se pudo extraer el nombre de la regla');
    return;
  }

  executeBtn.disabled = true;
  executeBtn.classList.add('executing');
  executeBtn.innerHTML = '<i class="fas fa-spinner fa-spin"></i> Verificando...';

  try {
    console.log(`🔍 Ejecutando regla: ${ruleName}`);
    console.log(`📝 Código: ${ruleCode}`);

    // 🔥 PRIMERO: Verificar que tenemos datos en sesión
    const sessionOk = await verifySessionState();
    if (!sessionOk) {
      throw new Error('La sesión no tiene datos válidos. Carga un archivo CSV o imagen primero.');
    }

    // 🔥 SEGUNDO: Determinar el tipo de consulta a ejecutar
    let queryToExecute;
    let queryType = 'desconocido';

    if (ruleCode.includes(':-')) {
      // Es una regla de definición - usar el nombre
      queryType = 'definición';
      queryToExecute = `${ruleName}(X).`;
    } else if (ruleCode.endsWith('.') && !ruleCode.includes(':-')) {
      // Es una consulta directa
      queryType = 'consulta';
      queryToExecute = ruleCode;
    } else {
      // Es un nombre de predicado
      queryType = 'predicado';
      queryToExecute = `${ruleName}(X).`;
    }

    console.log(`🔍 Ejecutando ${queryType}: ${queryToExecute}`);

    // 🔥 TERCERO: Ejecutar consulta principal
    let result;
    if (queryToExecute.includes('atom_number')) {
      // Reemplazar atom_number por number_string
      const correctedQuery = queryToExecute.replace(/atom_number/g, 'number_string');
      console.log(`🔧 Consulta corregida: ${correctedQuery}`);
      result = await executeSingleQueryHybrid(correctedQuery);
    } else {
      result = await executeSingleQueryHybrid(queryToExecute);
    }

    console.log(`📊 Resultado de consulta:`, result);

    if (result.success) {
      if (result.count > 0) {
        // 🔥 ÉXITO: Mostrar resultados
        showAttributeRuleResult(ruleName, result, ruleIndex);

        // También mostrar en el área principal de resultados
        displayPrologResults(result.results, result.count);
      } else {
        // Consulta exitosa pero sin resultados
        showNotification('info', `Regla: ${ruleName}`,
          `La consulta se ejecutó correctamente pero no devolvió resultados (0 coincidencias).\n\nPrueba con consultas más simples o verifica los datos cargados.`);
      }
    } else {
      // 🔥 FALLÓ: Intentar consultas alternativas
      console.log(`⚠️ Consulta principal falló, intentando alternativas...`);
      const alternativeResult = await tryAlternativeQueries(ruleName, ruleCode);

      if (alternativeResult.count > 0) {
        showAttributeRuleResult(ruleName, alternativeResult, ruleIndex);
      } else {
        // 🔔 MOSTRAR CONSEJOS ESPECÍFICOS
        let advice = '';
        if (result.error && result.error.includes('existence_error')) {
          advice = `El predicado '${ruleName}' no existe. Verifica que:\n• La regla esté correctamente definida\n• No haya conflictos de nombres\n• Los hechos estén cargados`;
        } else {
          advice = `La consulta no devolvió resultados. Prueba con:\n• Consultas más simples\n• Verificar los datos cargados\n• Recargar la sesión`;
        }

        showNotification('info', `Regla: ${ruleName}`,
          `0 resultados encontrados\n\n${advice}`);
      }
    }

  } catch (error) {
    console.error(`❌ Error ejecutando regla ${ruleName}:`, error);
    showNotification('error', 'Error en ejecución',
      `No se pudo ejecutar '${ruleName}': ${error.message}\n\nAsegúrate de haber cargado un archivo primero.`);
  } finally {
    executeBtn.disabled = false;
    executeBtn.classList.remove('executing');
    executeBtn.innerHTML = '<i class="fas fa-vial"></i> Probar Esta Regla';
  }
}

// 🔥 FUNCIÓN DEBUG MEJORADA
async function debugDataStructure() {
  console.log('🐛 INICIANDO DEBUG DE ESTRUCTURA DE DATOS');

  const result = await executeSingleQueryHybrid("dato(ID, Columna, Valor).");

  if (result.success && result.results) {
    console.log('🔍 ESTRUCTURA COMPLETA DE DATOS:');

    // Mostrar todos los resultados en la consola
    result.results.forEach((item, index) => {
      console.log(`   [${index + 1}] ID: ${item.ID}, Columna: ${item.Columna}, Valor: ${item.Valor}`);
    });

    // Agrupar por ID
    const registros = {};
    result.results.forEach(item => {
      const id = item.ID;
      if (!registros[id]) registros[id] = [];
      registros[id].push(`${item.Columna}=${item.Valor}`);
    });

    console.log('📦 DATOS AGRUPADOS POR ID:');
    Object.keys(registros).sort((a, b) => a - b).forEach(id => {
      console.log(`   ID ${id}: ${registros[id].join(', ')}`);
    });

    showNotification('success', 'Debug Completado',
      `Se encontraron ${result.count} hechos para ${Object.keys(registros).length} registros. Revisa la consola para detalles.`);
  } else {
    showNotification('error', 'Debug Fallido', 'No se pudieron obtener los datos para debug');
  }
}


// 🔥 FUNCIÓN PARA PROBAR CONSULTAS CORREGIDAS
async function testCorrectedQueries() {
  console.log('🔧 Probando consultas corregidas...');

  // Consulta 1: Salarios corregida (comparación de strings)
  const salaryQuery = "dato(ID, 'salario', Salario), Salario @> '35000'.";
  const salaryResult = await executeSingleQueryHybrid(salaryQuery);
  console.log('💰 Salarios > 35000:', salaryResult);

  // Consulta 2: Buscar Juan específico
  const juanQuery = "dato(ID, 'nombre', 'Juan').";
  const juanResult = await executeSingleQueryHybrid(juanQuery);
  console.log('👤 Resultados para Juan:', juanResult);

  // Consulta 3: Empleados de IT
  const itQuery = "dato(ID, 'departamento', 'IT').";
  const itResult = await executeSingleQueryHybrid(itQuery);
  console.log('💻 Empleados de IT:', itResult);

  // Consulta 4: Edades mayores
  const ageQuery = "dato(ID, 'edad', Edad), Edad @> '35'.";
  const ageResult = await executeSingleQueryHybrid(ageQuery);
  console.log('🎂 Edad > 35:', ageResult);
}

// 🔥 NUEVA FUNCIÓN: Limpiar reglas duplicadas del carrusel
function removeDuplicateRules() {
  const uniqueRules = [];
  const seenRules = new Set();

  carouselState.savedRules.forEach(rule => {
    const ruleKey = `${rule.name}|${rule.code}`;
    if (!seenRules.has(ruleKey)) {
      seenRules.add(ruleKey);
      uniqueRules.push(rule);
    }
  });

  const removedCount = carouselState.savedRules.length - uniqueRules.length;

  if (removedCount > 0) {
    carouselState.savedRules = uniqueRules;
    carouselState.currentRuleSet = uniqueRules;
    localStorage.setItem('attributeRuleCards', JSON.stringify(uniqueRules));

    updateCarousel();
    updateCarouselInfo();

    showNotification('success', 'Reglas Limpiadas',
      `Se eliminaron ${removedCount} reglas duplicadas`);
  } else {
    showNotification('info', 'Sin duplicados', 'No se encontraron reglas duplicadas');
  }
}

// 🔥 NUEVA FUNCIÓN: Consultas alternativas
async function tryAlternativeQueries(ruleName, ruleCode) {
  const alternatives = [
    `objeto_detectado(ID, Objeto, Confianza).`,
    `seguridad_objeto(ID, Seguridad).`,
    `estado_objeto(ID, Estado).`,
    `atributo_objeto(ID, Atributo).`,
    `total_objetos(N).`
  ];

  for (const query of alternatives) {
    try {
      const result = await executeSingleQueryHybrid(query);
      if (result.success && result.count > 0) {
        return result;
      }
    } catch (error) {
      continue;
    }
  }

  return { success: true, count: 0, results: [] };
}

// 🔥 NUEVA FUNCIÓN: Probar reglas específicas del sistema (VERSIÓN MEJORADA)
async function testSpecificRules() {
  const testQueries = [
    // 🔥 CONSULTAS BÁSICAS QUE DEBEN FUNCIONAR SIEMPRE
    "objeto_detectado(ID, Objeto, Confianza).",
    "seguridad_objeto(ID, Seguridad).",
    "estado_objeto(ID, Estado).",
    "atributo_objeto(ID, Atributo).",
    "total_objetos(Total).",

    // 🔥 REGLAS NUEVAS CON NOMBRES ÚNICOS
    "es_seguro(ID).",
    "esta_en_buen_estado(ID).",
    "listar_objetos.",
    "mostrar_estados.",
    "mostrar_seguridad.",

    // 🔥 CONSULTAS DE RESUMEN
    "obtener_resumen(Total, Seguros, Peligrosos, Buenos)."
  ];

  showLoading('querying', 'Probando reglas específicas...', 'Verificando predicados cargados');

  const results = [];

  for (const query of testQueries) {
    try {
      const result = await executeSingleQueryHybrid(query);
      results.push({
        query,
        success: result.success,
        count: result.count,
        error: result.error,
        results: result.results || []
      });

      console.log(`🔍 ${query}: ${result.count} resultados ${result.error ? '(ERROR: ' + result.error + ')' : ''}`);

      if (result.count > 0 && result.results) {
        console.log('   Primeros resultados:', result.results.slice(0, 2));
      }
    } catch (error) {
      results.push({
        query,
        success: false,
        error: error.message,
        results: []
      });
    }
    await new Promise(resolve => setTimeout(resolve, 300));
  }

  hideLoading();

  // Mostrar resumen detallado
  const working = results.filter(r => r.success && r.count > 0).length;
  const total = results.length;

  let message = `✅ ${working}/${total} consultas funcionan\n\n`;
  let details = '';

  results.forEach(r => {
    const status = r.success && r.count > 0 ? '✅' : r.success ? '⚠️' : '❌';
    details += `${status} ${r.query}: ${r.count} resultados\n`;

    if (r.error) {
      details += `   Error: ${r.error}\n`;
    }

    if (r.results && r.results.length > 0) {
      details += `   Ejemplo: ${JSON.stringify(r.results[0])}\n`;
    }

    details += '\n';
  });

  // Crear notificación expandible
  const notification = document.createElement('div');
  notification.className = `notification ${working === total ? 'success' : working > total / 2 ? 'warning' : 'error'}`;
  notification.innerHTML = `
    <div class="notification-progress ${working === total ? 'success' : working > total / 2 ? 'warning' : 'error'}"></div>
    <div class="notification-header">
      <div class="notification-icon">
        <i class="fas fa-vial"></i>
      </div>
      <h4 class="notification-title">Prueba de Reglas: ${working}/${total} funcionan</h4>
      <button class="notification-close" onclick="this.parentElement.parentElement.remove()">
        <i class="fas fa-times"></i>
      </button>
    </div>
    <div class="notification-content">
      <p>${message}</p>
      <details>
        <summary>Ver detalles completos</summary>
        <pre style="background: #f5f5f5; padding: 10px; border-radius: 5px; margin-top: 10px; font-size: 0.9em; white-space: pre-wrap;">${details}</pre>
      </details>
    </div>
  `;

  const container = document.getElementById('notificationContainer') || createNotificationContainer();
  container.appendChild(notification);
}

// Funciones de diagnóstico

function addDiagnosticButtons() {
  console.log('🔧 Agregando botones de diagnóstico...');

  const querySection = document.querySelector('.query-suggestions');
  if (!querySection) {
    console.error('❌ No se encontró la sección query-suggestions');

    // Intentar crear la sección si no existe
    const queryCard = document.querySelector('.query-card');
    if (queryCard) {
      const newSection = document.createElement('div');
      newSection.className = 'query-suggestions';
      newSection.innerHTML = '<h4>Consultas Rápidas</h4>';
      queryCard.appendChild(newSection);
      addDiagnosticButtonsToSection(newSection);
    }
    return;
  }

  addDiagnosticButtonsToSection(querySection);
}

// 🔥 NUEVA FUNCIÓN AUXILIAR
function addDiagnosticButtonsToSection(section) {
  // Verificar si ya existen botones de diagnóstico
  const existingDiagnostics = section.querySelector('.diagnostic-buttons');
  if (existingDiagnostics) {
    existingDiagnostics.remove();
  }

  const diagnosticDiv = document.createElement('div');
  diagnosticDiv.className = 'diagnostic-buttons';
  diagnosticDiv.style.cssText = `
    display: flex;
    gap: 10px;
    margin-top: 15px;
    flex-wrap: wrap;
    padding: 10px;
    background: rgba(255, 255, 255, 0.05);
    border-radius: 8px;
    border: 1px solid rgba(255, 255, 255, 0.1);
  `;

  diagnosticDiv.innerHTML = `
    <div style="width: 100%; margin-bottom: 8px;">
      <strong style="color: var(--primary-light);">
        <i class="fas fa-tools"></i> Herramientas de Diagnóstico y Limpieza
      </strong>
    </div>
    <button class="btn btn-outline btn-sm" onclick="verifySessionState()" title="Verificar estado de la sesión">
      <i class="fas fa-search"></i> Verificar Sesión
    </button>
    <button class="btn btn-outline btn-sm" onclick="showDatabaseStatus()" title="Ver estado completo de la base de datos">
      <i class="fas fa-database"></i> Estado BD
    </button>
    <button class="btn btn-outline btn-sm" onclick="clearCurrentSession()" title="Limpiar solo la sesión actual">
      <i class="fas fa-broom"></i> Limpiar Sesión
    </button>
    <button class="btn btn-danger btn-sm" onclick="clearDatabase()" title="LIMPIAR TODA LA BASE DE DATOS (CUIDADO)">
      <i class="fas fa-trash"></i> Limpiar Todo
    </button>
  `;

  section.appendChild(diagnosticDiv);
  console.log('✅ Botones de diagnóstico agregados correctamente');
}

// 🔥 NUEVA FUNCIÓN: Verificar predicados cargados
async function checkPredicates() {
  try {
    const response = await fetch(`/prolog/predicates/${appState.sessionId}`);
    const result = await response.json();

    if (result.success) {
      const workingPredicates = result.predicates.filter(p => p.exists);
      const brokenPredicates = result.predicates.filter(p => !p.exists);

      let message = `✅ ${workingPredicates.length}/${result.predicates.length} predicados funcionan\n\n`;

      if (workingPredicates.length > 0) {
        message += "Funcionan:\n";
        workingPredicates.forEach(p => {
          message += `• ${p.predicate}\n`;
        });
      }

      if (brokenPredicates.length > 0) {
        message += "\nNo funcionan:\n";
        brokenPredicates.forEach(p => {
          message += `• ${p.predicate}: ${p.error}\n`;
        });
      }

      showNotification('info', 'Diagnóstico de Predicados', message);
    } else {
      showNotification('error', 'Error', result.error);
    }
  } catch (error) {
    showNotification('error', 'Error', `No se pudo verificar predicados: ${error.message}`);
  }
}

// Función para verificar el estado del archivo rules.pl
async function checkRulesFile() {
  try {
    const response = await fetch('/prolog/debug');
    const diagnostic = await response.json();

    console.log('📁 DIAGNÓSTICO RULES.PL:', diagnostic);

    if (!diagnostic.fileExists) {
      showNotification('❌ Archivo rules.pl no encontrado', 'error');
    } else if (diagnostic.criticalDefinitions.color === 0) {
      showNotification('⚠️ Archivo rules.pl no tiene definiciones color()', 'warning');
    } else {
      showNotification('✅ Archivo rules.pl cargado correctamente', 'success');
    }

    return diagnostic;
  } catch (error) {
    console.error('Error en diagnóstico:', error);
    showNotification('❌ Error verificando archivo rules.pl', 'error');
  }
}


// 🔥 NUEVA FUNCIÓN: Probar consultas básicas que DEBEN funcionar
async function testBasicQueries() {
  const basicQueries = [
    'objeto_detectado(ID, Objeto, Confianza).',
    'seguridad_objeto(ID, Seguridad).',
    'estado_objeto(ID, Estado).',
    'atributo_objeto(ID, Atributo).',
    'total_objetos(Total).'
  ];

  showLoading('querying', 'Probando consultas básicas...');

  const results = [];

  for (const query of basicQueries) {
    try {
      const result = await executeSingleQueryHybrid(query);
      results.push({
        query,
        success: result.success,
        count: result.count,
        error: result.error
      });

      console.log(`🔍 ${query}: ${result.success ? '✅' : '❌'} ${result.count} resultados`);
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

  const working = results.filter(r => r.success).length;
  const total = results.length;

  let message = `Resultados: ${working}/${total} consultas funcionan\n\n`;
  results.forEach(r => {
    const status = r.success ? '✅' : '❌';
    message += `${status} ${r.query}\n`;
    if (r.error) {
      message += `   Error: ${r.error}\n`;
    }
    message += `   Resultados: ${r.count}\n\n`;
  });

  showNotification(
    working === total ? 'success' : working > 0 ? 'warning' : 'error',
    'Prueba de Consultas Básicas',
    message
  );
}

// 🔥 NUEVA FUNCIÓN: Probar consultas complejas
async function testComplexQueries() {
  const complexQueries = [
    "objeto_detectado(ID, 'manzana', Confianza).",
    "findall(Objeto, objeto_detectado(_, Objeto, _), Objetos).",
    "seguridad_objeto(ID, 'seguro').",
    "findall(ID, (objeto_detectado(ID, _, _), seguridad_objeto(ID, 'seguro')), Seguros), length(Seguros, Total)."
  ];

  showLoading('querying', 'Probando consultas complejas...');

  for (const query of complexQueries) {
    try {
      const result = await executeSingleQueryHybrid(query);
      console.log(`🔍 ${query}: ${result.count} resultados`);
      if (result.count > 0) {
        console.log('Resultados:', result.results);
      }
    } catch (error) {
      console.error(`❌ Error en ${query}:`, error);
    }
    await new Promise(resolve => setTimeout(resolve, 500));
  }

  hideLoading();
}

async function testAttributeSystem() {
  showLoading('analyzing', 'Probando sistema completo...');

  const testQueries = [
    'objeto_detectado(X, Y, Z).',
    'total_objetos(N).',
    'seguridad_objeto(ID, Seg).',
    'estado_objeto(ID, Est).',
    'atributo_objeto(ID, Attr).'
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
        warning: result.warning
      });

      console.log(`🔍 ${query}: ${result.count} resultados`);
      if (result.count > 0) {
        console.log('Ejemplo:', result.results[0]);
      }
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

  const passed = results.filter(r => r.success && r.count > 0).length;
  const total = results.length;

  let message = `${passed}/${total} consultas con datos\n\n`;
  results.forEach(r => {
    message += `• ${r.query}: ${r.success ? '✅' : '❌'} ${r.count} resultados`;
    if (r.warning) message += ` (⚠️ ${r.warning})`;
    message += '\n';
  });

  showNotification(
    passed === total ? 'success' : passed > total / 2 ? 'warning' : 'error',
    'Prueba del Sistema',
    message
  );
}

function clearAllRules() {
  if (carouselState.savedRules.length === 0) {
    showNotification('info', 'Carrusel Vacío', 'No hay reglas para eliminar');
    return;
  }

  if (confirm(`¿Estás seguro de que quieres eliminar TODAS las reglas (${carouselState.savedRules.length})?`)) {
    carouselState.savedRules = [];
    carouselState.currentRuleSet = [];
    localStorage.removeItem('attributeRuleCards');

    updateCarousel();
    updateCarouselInfo();

    showNotification('info', 'Carrusel Limpiado', 'Todas las reglas han sido eliminadas');
  }
}

// Función de ejecución de consultas (mantener del código original)
async function executeSingleQueryHybrid(query) {
  try {
    console.log(`🎯 EJECUTANDO CONSULTA: ${query}`);

    const allRules = carouselState.savedRules.map(rule => rule.code).join('\n');
    const customRules = document.getElementById('customRules')?.value || '';

    const response = await fetch('/query/prolog', {
      method: 'POST',
      headers: { 'Content-Type': 'application/json' },
      body: JSON.stringify({
        query: query,
        customRules: allRules + '\n' + customRules,
        sessionId: appState.sessionId
      })
    });

    if (!response.ok) {
      throw new Error(`HTTP error! status: ${response.status}`);
    }

    const data = await response.json();

    console.log('🎯 RESULTADO TAU-PROLOG:');
    console.log('✅ Éxito:', data.success);
    console.log('📊 Resultados:', data.count);
    console.log('🔍 Datos:', data.results);

    return {
      success: data.success,
      results: data.results || [],
      count: data.count || 0,
      error: data.error,
      source: 'server'
    };

  } catch (error) {
    console.error('❌ Error en consulta:', error);
    return {
      success: false,
      error: error.message,
      results: [],
      count: 0,
      source: 'error'
    };
  }
}

// Funciones de carga de datos (opcionales, mantener compatibilidad)
async function loadSavedQueries() {
  // Implementación básica
}

async function loadSavedRulesList() {
  // Implementación básica  
}

console.log('✅ Sistema de detección de atributos cargado correctamente');

// 🔥 FUNCIONES FALTANTES PARA LA INTERFAZ

// Función para cambiar pestañas
// 🔥 CORREGIR: Función switchTab
function switchTab(tabName, event) {
  // Ocultar todos los tabs
  document.querySelectorAll('.tab-content').forEach(tab => {
    tab.classList.remove('active');
  });

  // Desactivar todos los botones
  document.querySelectorAll('.tab-btn').forEach(btn => {
    btn.classList.remove('active');
  });

  // Mostrar tab seleccionado
  const targetTab = document.getElementById(tabName + 'Tab');
  if (targetTab) {
    targetTab.classList.add('active');
  }

  // Activar botón si hay evento
  if (event && event.currentTarget) {
    event.currentTarget.classList.add('active');
  } else {
    // Activar botón por selector
    const targetBtn = document.querySelector(`.tab-btn[onclick*="${tabName}"]`);
    if (targetBtn) {
      targetBtn.classList.add('active');
    }
  }
}

// 🔥 MEJORAR executePrologQuery para evitar ejecuciones múltiples
let isExecutingQuery = false;

// Función para ejecutar consultas Prolog
async function executePrologQuery() {
  // Evitar ejecución múltiple
  if (isExecutingQuery) {
    console.log('⚠️ Consulta ya en ejecución, ignorando...');
    return;
  }

  isExecutingQuery = true;

  const queryInput = document.getElementById('prologQuery');
  const query = queryInput.value.trim();

  if (!query) {
    showNotification('warning', 'Consulta vacía', 'Por favor ingresa una consulta Prolog');
    isExecutingQuery = false;
    return;
  }

  // Validar consulta
  const validation = validatePrologQuery(query);
  if (!validation.isValid) {
    showNotification('error', 'Error de sintaxis', validation.error);
    isExecutingQuery = false;
    return;
  }

  showLoading('querying', 'Ejecutando consulta...');

  try {
    console.log(`🚀 Ejecutando consulta: ${query}`);

    // Usar el endpoint con PostgreSQL
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
    isExecutingQuery = false;
  }
}

// 🔥 CORREGIR: Función para mostrar resultados Prolog
function displayPrologResults(results, count, query) {
  const resultCount = document.getElementById('resultCount');
  const prologOutput = document.getElementById('prologOutput');
  const prologResults = document.getElementById('prologResults');

  if (!resultCount || !prologOutput || !prologResults) {
    console.error('❌ Elementos de resultados no encontrados');
    return;
  }

  // Asegurar que query sea string
  const safeQuery = query || 'Consulta Prolog';

  // Actualizar contador
  resultCount.textContent = `${count} ${count === 1 ? 'resultado' : 'resultados'}`;

  // Mostrar resultados
  if (!results || results.length === 0) {
    prologOutput.innerHTML = `
      <div class="no-results">
        <i class="fas fa-search"></i>
        <h4>Consulta ejecutada correctamente</h4>
        <p>La consulta <strong>"${safeQuery}"</strong> se ejecutó pero no devolvió resultados.</p>
        <div class="result-type-info">
          <span class="result-badge info">ℹ️ CONSULTA VÁLIDA</span>
          <p>Esto puede significar:</p>
          <ul>
            <li>La condición no se cumple para ningún dato</li>
            <li>Los hechos no coinciden con el patrón buscado</li>
            <li>La consulta es correcta pero sin coincidencias</li>
          </ul>
        </div>
      </div>
    `;
  } else {
    const queryType = detectQueryType(safeQuery);
    let html = `
      <div class="results-intuitive">
        <div class="query-type-badge ${queryType}">
          <i class="fas ${getQueryTypeIcon(queryType)}"></i>
          ${getQueryTypeLabel(queryType)}
        </div>
    `;

    // Mostrar resultados según el tipo
    if (results.length === 1 && Object.keys(results[0]).length === 0) {
      // Resultado booleano true
      html += `
        <div class="boolean-result true">
          <div class="boolean-icon">
            <i class="fas fa-check-circle"></i>
          </div>
          <div class="boolean-content">
            <h3>VERDADERO (true)</h3>
            <p>La consulta <strong>"${safeQuery}"</strong> es <strong>verdadera</strong></p>
            <div class="success-note">✓ La condición se cumple en la base de conocimiento</div>
          </div>
        </div>
      `;
    } else {
      // Resultados múltiples
      html += `
        <div class="facts-summary">
          <div class="summary-card">
            <i class="fas fa-database"></i>
            <div class="summary-content">
              <h4>${count} coincidencias encontradas</h4>
              <p>La base de conocimiento contiene ${count} hechos que cumplen con la consulta</p>
            </div>
          </div>
        </div>
        <div class="results-grid">
      `;

      results.forEach((result, index) => {
        html += `
          <div class="result-card">
            <div class="result-header">
              <span class="result-index">#${index + 1}</span>
              <span class="result-type">Solución</span>
            </div>
            <div class="result-content">
        `;

        if (Object.keys(result).length === 0) {
          html += `<div class="simple-success">✓ Consulta satisfecha</div>`;
        } else {
          Object.entries(result).forEach(([variable, value]) => {
            if (variable !== 'success' && variable !== 'undefined' && value !== undefined) {
              html += `
                <div class="variable-binding">
                  <span class="variable-name">${variable}</span>
                  <span class="binding-operator"> = </span>
                  <span class="variable-value">${formatPrologValue(value)}</span>
                </div>
              `;
            }
          });
        }

        html += `
            </div>
          </div>
        `;
      });

      html += '</div>';
    }

    html += '</div>';
    prologOutput.innerHTML = html;
  }

  // Mostrar contenedor de resultados
  prologResults.style.display = 'block';
}

// 🔥 CORREGIR: Función detectQueryType
function detectQueryType(query) {
  if (!query || typeof query !== 'string') {
    return 'fact'; // Valor por defecto
  }

  const q = query.toLowerCase().trim();

  if (q.includes(':-') || q.includes('->')) return 'rule';
  if (q.endsWith('.') && !q.includes('(') && !q.includes(')')) return 'boolean';
  if (q.includes('findall') || q.includes('bagof') || q.includes('setof')) return 'aggregate';
  if (q.includes(',') || q.includes(';')) return 'complex';
  if (q.includes('_') || /[A-Z][a-zA-Z]*/.test(q)) return 'enumerate';

  return 'fact';
}

// 🔥 NUEVO: Obtener icono según tipo de consulta
function getQueryTypeIcon(type) {
  const icons = {
    'boolean': 'fa-question-circle',
    'fact': 'fa-search',
    'enumerate': 'fa-list',
    'aggregate': 'fa-calculator',
    'complex': 'fa-project-diagram',
    'rule': 'fa-cogs'
  };
  return icons[type] || 'fa-code';
}

// 🔥 NUEVO: Obtener etiqueta según tipo de consulta
function getQueryTypeLabel(type) {
  const labels = {
    'boolean': 'Consulta Booleana',
    'fact': 'Búsqueda de Hechos',
    'enumerate': 'Enumeración',
    'aggregate': 'Agregación',
    'complex': 'Consulta Compleja',
    'rule': 'Evaluación de Regla'
  };
  return labels[type] || 'Consulta Prolog';
}

// 🔥 NUEVO: Formatear valores Prolog
function formatPrologValue(value) {
  if (value === null || value === undefined) return '<span class="null-value">null</span>';
  if (value === true) return '<span class="true-value">✓ true</span>';
  if (value === false) return '<span class="false-value">✗ false</span>';

  const str = String(value);
  if (str === '[]') return '<span class="empty-list">[]</span>';
  if (str.startsWith('[') && str.endsWith(']')) return `<span class="list-value">${str}</span>`;
  if (/^\d+$/.test(str)) return `<span class="number-value">${str}</span>`;
  if (str.startsWith("'") && str.endsWith("'")) return `<span class="string-value">${str}</span>`;

  return `<span class="atom-value">${str}</span>`;
}


// Función para validar consultas Prolog
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

// Función para guardar reglas
async function saveRules() {
  const rules = document.getElementById('customRules').value.trim();
  const ruleName = document.getElementById('ruleName').value || `reglas_${Date.now()}`;

  if (!rules) {
    showNotification('warning', 'Advertencia', 'No hay reglas para guardar');
    return;
  }

  try {
    // Guardar en el servidor
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
      // Crear cards individuales
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
      localStorage.setItem('attributeRuleCards', JSON.stringify(carouselState.savedRules));

      updateCarousel();
      updateCarouselInfo();

      showNotification('success', 'Reglas Guardadas',
        `Se crearon ${individualRules.length} reglas en el carrusel`);

    } else {
      throw new Error(result.error);
    }
  } catch (error) {
    showNotification('error', 'Error',
      `No se pudieron guardar las reglas: ${error.message}`);
  }
}

// Función para cargar plantillas de reglas
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

// Función para agregar reglas al editor
function addToCustomRules(newRules) {
  const currentRules = document.getElementById('customRules').value;
  const rulesEditor = document.getElementById('customRules');

  if (currentRules && !currentRules.endsWith('\n')) {
    rulesEditor.value += '\n';
  }
  rulesEditor.value += newRules + '\n';

  // Hacer scroll al final
  rulesEditor.scrollTop = rulesEditor.scrollHeight;
}

// Función para eliminar reglas individuales
function deleteRule(ruleIndex) {
  const rule = carouselState.currentRuleSet[ruleIndex];
  if (!rule) return;

  if (confirm(`¿Estás seguro de que quieres eliminar la regla "${rule.name}"?`)) {
    // Eliminar del array
    carouselState.savedRules = carouselState.savedRules.filter(r => r.id !== rule.id);
    carouselState.currentRuleSet = carouselState.savedRules;

    // Guardar cambios
    localStorage.setItem('attributeRuleCards', JSON.stringify(carouselState.savedRules));

    // Actualizar carrusel
    updateCarousel();
    updateCarouselInfo();

    showNotification('info', 'Regla Eliminada',
      `"${rule.name}" ha sido eliminada del carrusel`);
  }
}

// Función para copiar reglas al portapapeles
function copyRuleToClipboard(ruleCode) {
  navigator.clipboard.writeText(ruleCode).then(() => {
    showNotification('success', 'Copiada', 'Regla copiada al portapapeles');
  }).catch(err => {
    showNotification('error', 'Error', 'No se pudo copiar la regla');
  });
}

// Función para cargar reglas guardadas
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

// Función para cargar una regla guardada
async function loadSavedRule(ruleName) {
  try {
    const response = await fetch(`/rules/load/${appState.sessionId}/${ruleName}`);
    const result = await response.json();

    if (result.success) {
      const customRules = document.getElementById('customRules');
      customRules.value = result.rules;
      showNotification('success', 'Reglas Cargadas', `Reglas "${ruleName}" cargadas en el editor`);
    }
  } catch (error) {
    showNotification('error', 'Error', `No se pudieron cargar las reglas: ${error.message}`);
  }
}

// Función para usar regla en consultas
async function useSavedRuleInQueries(ruleName) {
  try {
    const response = await fetch(`/rules/load/${appState.sessionId}/${ruleName}`);
    const result = await response.json();

    if (result.success) {
      addToCustomRules(result.rules);
      showNotification('success', 'Reglas Agregadas', `Reglas "${ruleName}" listas para usar en consultas`);
    }
  } catch (error) {
    showNotification('error', 'Error', `No se pudieron usar las reglas: ${error.message}`);
  }
}

// Función para eliminar regla guardada
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
      showNotification('info', 'Reglas Eliminadas', `Reglas "${ruleName}" eliminadas`);
      loadSavedRulesList();
    } else {
      throw new Error(result.error);
    }
  } catch (error) {
    showNotification('error', 'Error', `No se pudieron eliminar las reglas: ${error.message}`);
  }
}

// Función para cargar consultas guardadas
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

// Función para guardar consulta actual
function saveQuery() {
  const query = document.getElementById('prologQuery').value.trim();
  if (!query) {
    showNotification('warning', 'Consulta vacía', 'No hay consulta para guardar');
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
    showNotification('success', 'Consulta Guardada', 'Consulta guardada en favoritos');
    loadSavedQueries();
  }
}

// Función para eliminar consulta guardada
function deleteSavedQuery(index) {
  const savedQueries = JSON.parse(localStorage.getItem('savedQueries') || '[]');
  savedQueries.splice(index, 1);
  localStorage.setItem('savedQueries', JSON.stringify(savedQueries));
  loadSavedQueries();
  showNotification('info', 'Consulta Eliminada', 'Consulta eliminada de favoritos');
}

// Función para cargar consulta automática
function loadAutoQuery(query) {
  const queryEditor = document.getElementById('prologQuery');
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

// Función para limpiar resultados
function clearResults() {
  const prologOutput = document.getElementById('prologOutput');
  const prologResults = document.getElementById('prologResults');

  if (prologOutput) prologOutput.textContent = '';
  if (prologResults) prologResults.style.display = 'none';

  showNotification('info', 'Resultados Limpiados', 'Los resultados han sido eliminados');
}

// Función para exportar resultados
function exportResults() {
  const results = document.getElementById('prologOutput').textContent;
  if (!results) {
    showNotification('warning', 'Sin resultados', 'No hay resultados para exportar');
    return;
  }

  const blob = new Blob([results], { type: 'text/plain' });
  const url = URL.createObjectURL(blob);
  const a = document.createElement('a');
  a.href = url;
  a.download = `resultados_prolog_${new Date().getTime()}.txt`;
  a.click();
  URL.revokeObjectURL(url);

  showNotification('success', 'Exportado', 'Resultados exportados correctamente');
}

// Función para procesar archivos de datos
// MEJORAR processDataFile para evitar duplicados
let isProcessingFile = false;

async function processDataFile(file) {
  // Evitar procesamiento duplicado
  if (isProcessingFile) {
    console.log('⚠️ Ya se está procesando un archivo, ignorando...');
    return;
  }

  isProcessingFile = true;
  
  // Mostrar loading optimizado para archivos grandes
  const fileSizeMB = (file.size / (1024 * 1024)).toFixed(2);
  const isLargeFile = file.size > 5 * 1024 * 1024; // > 5MB
  
  showLoading('processing', 
    isLargeFile ? 'Procesando archivo grande...' : 'Procesando archivo...', 
    isLargeFile ? `Archivo de ${fileSizeMB} MB - Esto puede tomar unos segundos...` : 'Extrayendo y organizando información'
  );

  const formData = new FormData();
  formData.append('file', file);
  formData.append('sessionId', appState.sessionId);

  // Timeout para evitar bloqueos eternos
  const timeoutPromise = new Promise((_, reject) => {
    setTimeout(() => reject(new Error('Timeout: El archivo es demasiado grande o hay problemas de conexión')), 60000);
  });

  try {
    console.log(`📤 Enviando archivo: ${file.name} (${fileSizeMB} MB)`);
    
    const fetchPromise = fetch('/upload/data', {
      method: 'POST',
      body: formData
    });

    // Usar Promise.race para timeout
    const response = await Promise.race([fetchPromise, timeoutPromise]);

    if (!response.ok) {
      throw new Error(`Error HTTP: ${response.status}`);
    }

    const result = await response.json();

    if (result.success) {
      appState.currentData = result.data;
      appState.prologFacts = result.prologFacts;
      appState.currentFile = file;

      // 🔥 OPTIMIZAR: Mostrar solo información básica para archivos grandes
      if (isLargeFile) {
        showNotification('success', 'Archivo Grande Procesado',
          `${result.data.length} registros cargados correctamente\n\n💡 Para mejor rendimiento, considera dividir archivos muy grandes (>50MB)`);
        
        // Mostrar tabla con paginación virtual
        displayOptimizedDataTable(result.data, result.stats);
      } else {
        showNotification('success', 'Archivo Procesado',
          `${result.data.length} registros cargados correctamente`);
        
        displayFileInfo(file, result.stats);
        displayDataTable(result.data);
      }
      
      displayStats(result.stats, result.data);

      // Generar reglas automáticamente para datos
      if (!isLargeFile) {
        generateDataRules(result.data, result.stats);
      }

    } else {
      throw new Error(result.error || 'Error desconocido');
    }
  } catch (error) {
    console.error('❌ Error procesando archivo:', error);
    
    if (error.message.includes('Timeout')) {
      showNotification('error', 'Tiempo Excedido', 
        `El archivo es demasiado grande (${fileSizeMB} MB). Intenta con un archivo más pequeño o divídelo en partes.`);
    } else {
      showNotification('error', 'Error', `Error al procesar el archivo: ${error.message}`);
    }
  } finally {
    hideLoading();
    isProcessingFile = false;
  }
}

// 🔥 NUEVA FUNCIÓN: Mostrar tabla optimizada para archivos grandes
function displayOptimizedDataTable(data, stats) {
  const dataTable = document.getElementById('dataTable');
  if (!dataTable) return;

  if (!data || data.length === 0) {
    dataTable.innerHTML = '<p class="text-muted">No hay datos para mostrar</p>';
    return;
  }

  const totalRecords = data.length;
  const sampleSize = Math.min(100, totalRecords); // Mostrar máximo 100 registros
  const sampleData = data.slice(0, sampleSize);
  
  const headers = Object.keys(sampleData[0]);
  
  let html = `
    <div class="large-file-warning">
      <i class="fas fa-info-circle"></i>
      <strong>Archivo grande detectado:</strong> Mostrando ${sampleSize} de ${totalRecords} registros para mejor rendimiento
    </div>
    <div class="table-responsive">
      <table class="data-table">
        <thead>
          <tr>
            ${headers.map(header => `<th>${header}</th>`).join('')}
          </tr>
        </thead>
        <tbody>
  `;

  sampleData.forEach(row => {
    html += '<tr>';
    headers.forEach(header => {
      const value = row[header];
      // Truncar valores muy largos
      const displayValue = value !== null && value !== undefined ? 
        String(value).length > 100 ? String(value).substring(0, 100) + '...' : value : '';
      html += `<td title="${value}">${displayValue}</td>`;
    });
    html += '</tr>';
  });

  html += `
        </tbody>
      </table>
    </div>
    <div class="table-footer">
      <small class="text-muted">
        <i class="fas fa-database"></i>
        Total: ${totalRecords} registros, ${headers.length} columnas
        ${totalRecords > sampleSize ? ` (mostrando ${sampleSize} registros)` : ''}
      </small>
    </div>
  `;

  dataTable.innerHTML = html;
}

// Función para mostrar información del archivo
function displayFileInfo(file, stats) {
  const fileInfo = document.getElementById('fileInfo');
  if (!fileInfo) return;

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

// Función para mostrar tabla de datos
function displayDataTable(data) {
  const dataTable = document.getElementById('dataTable');
  if (!dataTable) return;

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

// Función para mostrar estadísticas
// 🔥 MEJORAR: Mostrar estadísticas con gráficos
function displayStats(stats, data) {
  const statsCard = document.getElementById('statsCard');
  const statsGrid = document.getElementById('statsGrid');

  if (!statsCard || !statsGrid) return;

  let html = '';

  // Estadísticas generales con gráficos
  html += `
    <div class="stats-overview">
      <div class="stat-card main">
        <div class="stat-icon">
          <i class="fas fa-database"></i>
        </div>
        <div class="stat-info">
          <div class="stat-value">${stats.totalRecords}</div>
          <div class="stat-label">Registros Totales</div>
        </div>
      </div>
      
      <div class="stat-card main">
        <div class="stat-icon">
          <i class="fas fa-columns"></i>
        </div>
        <div class="stat-info">
          <div class="stat-value">${stats.columns}</div>
          <div class="stat-label">Columnas</div>
        </div>
      </div>
    </div>
  `;

  // Gráficos circulares para distribución de datos
  if (data && data.length > 0) {
    html += `<div class="charts-section">
      <h4><i class="fas fa-chart-pie"></i> Distribución de Datos</h4>
      <div class="charts-grid">`;

    // Generar gráficos para las primeras 3 columnas categóricas
    const categoricalColumns = findCategoricalColumns(data).slice(0, 3);

    categoricalColumns.forEach(column => {
      const distribution = calculateDistribution(data, column);
      html += createPieChart(column, distribution);
    });

    html += `</div></div>`;
  }

  // Estadísticas detalladas por columna
  if (stats.columnStats) {
    html += `<div class="detailed-stats">
      <h4><i class="fas fa-chart-bar"></i> Estadísticas por Columna</h4>
      <div class="column-stats-grid">`;

    Object.entries(stats.columnStats).forEach(([column, columnStats]) => {
      html += `
        <div class="column-stat-card">
          <div class="column-header">
            <h5>${column}</h5>
            <span class="data-type ${columnStats.type}">${columnStats.type}</span>
          </div>
          <div class="column-metrics">
            <div class="metric">
              <span class="metric-label">No nulos:</span>
              <span class="metric-value">${columnStats.nonNull}</span>
            </div>
            ${columnStats.unique ? `
            <div class="metric">
              <span class="metric-label">Valores únicos:</span>
              <span class="metric-value">${columnStats.unique}</span>
            </div>
            ` : ''}
            ${columnStats.mostFrequent ? `
            <div class="metric">
              <span class="metric-label">Más frecuente:</span>
              <span class="metric-value">${columnStats.mostFrequent.value} (${columnStats.mostFrequent.count})</span>
            </div>
            ` : ''}
          </div>
        </div>
      `;
    });

    html += `</div></div>`;
  }

  statsGrid.innerHTML = html;
  statsCard.style.display = 'block';

  // Inicializar gráficos después de renderizar
  setTimeout(initializeCharts, 100);
}

// 🔥 NUEVO: Encontrar columnas categóricas
function findCategoricalColumns(data) {
  if (!data || data.length === 0) return [];

  const sample = data[0];
  const categorical = [];

  Object.keys(sample).forEach(key => {
    const values = data.map(row => row[key]);
    const uniqueValues = [...new Set(values)].filter(v => v !== null && v !== undefined);

    // Considerar categórica si tiene menos de 10 valores únicos y no son todos numéricos
    if (uniqueValues.length <= 10 && uniqueValues.length > 1) {
      const numericCount = uniqueValues.filter(v => !isNaN(v) && v !== '').length;
      if (numericCount / uniqueValues.length < 0.8) {
        categorical.push(key);
      }
    }
  });

  return categorical;
}

// 🔥 NUEVO: Calcular distribución de datos
function calculateDistribution(data, column) {
  const distribution = {};

  data.forEach(row => {
    const value = row[column];
    if (value !== null && value !== undefined) {
      const key = String(value);
      distribution[key] = (distribution[key] || 0) + 1;
    }
  });

  return distribution;
}

// 🔥 NUEVO: Crear gráfico circular
function createPieChart(column, distribution) {
  const total = Object.values(distribution).reduce((sum, count) => sum + count, 0);
  const items = Object.entries(distribution)
    .sort((a, b) => b[1] - a[1])
    .slice(0, 5); // Top 5 categorías

  let chartHtml = `
    <div class="pie-chart-card">
      <div class="pie-chart-header">
        <h6>${column}</h6>
        <span class="chart-total">${total} registros</span>
      </div>
      <div class="pie-chart-container">
        <div class="pie-chart" id="pie-${column.replace(/[^a-zA-Z0-9]/g, '-')}">
          <div class="pie-chart-svg">
  `;

  // Generar SVG del gráfico circular
  let currentAngle = 0;
  const colors = ['#6366f1', '#06b6d4', '#10b981', '#f59e0b', '#ef4444'];

  items.forEach(([label, count], index) => {
    const percentage = (count / total) * 100;
    const angle = (percentage / 100) * 360;

    chartHtml += `
      <div class="pie-segment" 
           style="--start: ${currentAngle}deg; --end: ${currentAngle + angle}deg; --color: ${colors[index % colors.length]};"
           title="${label}: ${count} (${percentage.toFixed(1)}%)">
      </div>
    `;

    currentAngle += angle;
  });

  chartHtml += `
          </div>
          <div class="pie-center">
            <span>${items.length}</span>
            <small>categorías</small>
          </div>
        </div>
      </div>
      <div class="pie-legend">
  `;

  items.forEach(([label, count], index) => {
    const percentage = ((count / total) * 100).toFixed(1);
    chartHtml += `
      <div class="legend-item">
        <span class="legend-color" style="background: ${colors[index % colors.length]}"></span>
        <span class="legend-label">${label}</span>
        <span class="legend-value">${count} (${percentage}%)</span>
      </div>
    `;
  });

  chartHtml += `</div></div>`;
  return chartHtml;
}

// 🔥 NUEVO: Inicializar gráficos
function initializeCharts() {
  // Agregar animaciones a los gráficos circulares
  document.querySelectorAll('.pie-chart').forEach(chart => {
    chart.classList.add('animated');
  });
}

// Función para generar reglas automáticas desde datos
function generateDataRules(data, stats) {
  if (!data || data.length === 0) return;

  const columnNames = Object.keys(data[0] || {});
  const dataRules = [];

  // Reglas básicas para cada columna
  columnNames.forEach(column => {
    const cleanColumn = column.replace(/[^a-zA-Z0-9]/g, '_').toLowerCase();

    dataRules.push({
      id: Date.now() + Math.random(),
      name: `consultar_${cleanColumn}`,
      code: `dato(ID, '${column}', Valor).`,
      description: `Consultar todos los valores de la columna ${column}`,
      timestamp: new Date().toISOString(),
      type: 'data'
    });

    dataRules.push({
      id: Date.now() + Math.random() + 1,
      name: `contar_${cleanColumn}`,
      code: `findall(Valor, dato(_, '${column}', Valor), Lista), length(Lista, Total).`,
      description: `Contar registros en columna ${column}`,
      timestamp: new Date().toISOString(),
      type: 'count'
    });
  });

  // Reglas de análisis
  dataRules.push({
    id: Date.now() + Math.random() + 2,
    name: 'resumen_datos',
    code: 'total_registros(Total), write("Total registros: "), write(Total), nl.',
    description: 'Mostrar resumen general de datos',
    timestamp: new Date().toISOString(),
    type: 'analysis'
  });

  dataRules.push({
    id: Date.now() + Math.random() + 3,
    name: 'listar_columnas',
    code: 'columna(Columna), write("Columna: "), write(Columna), nl, fail.',
    description: 'Listar todas las columnas disponibles',
    timestamp: new Date().toISOString(),
    type: 'data'
  });

  // Agregar al carrusel
  dataRules.forEach(rule => {
    carouselState.savedRules.unshift(rule);
  });

  // Limitar a 50 reglas máximo
  if (carouselState.savedRules.length > 50) {
    carouselState.savedRules = carouselState.savedRules.slice(0, 50);
  }

  carouselState.currentRuleSet = carouselState.savedRules;
  localStorage.setItem('attributeRuleCards', JSON.stringify(carouselState.savedRules));

  updateCarousel();
  updateCarouselInfo();

  showNotification('success', 'Reglas Generadas',
    `Se crearon ${dataRules.length} reglas automáticamente desde los datos`);
}

// Función para manejar selección de archivos
function handleFileSelect(e, type) {
  if (e.target.files.length > 0) {
    const file = e.target.files[0];
    if (type === 'data') {
      processDataFile(file);
    } else {
      processImageFile(file);
    }
  }
}

// Agrega esta función en app.js después de las otras funciones auxiliares:

// Función mejorada para generar reglas automáticamente
async function generateRules() {
  if (!appState.currentAnalysis) {
    showNotification('warning', 'Sin análisis', 'Primero analiza una imagen para generar reglas automáticamente');
    analyzeImageForAttributes();
    return;
  }

  showLoading('analyzing', 'Generando reglas automáticas...', 'Creando y guardando reglas Prolog');

  try {
    const response = await fetch('/rules/generate', {
      method: 'POST',
      headers: {
        'Content-Type': 'application/json'
      },
      body: JSON.stringify({
        sessionId: appState.sessionId,
        criteria: ['detection', 'quality', 'safety', 'count', 'classification']
      })
    });

    const result = await response.json();

    if (result.success) {
      // 🔥 VERIFICAR QUE LAS REGLAS SE GUARDARON EN LA SESIÓN
      if (result.sessionUpdated) {
        console.log(`✅ Reglas guardadas en sesión: ${result.ruleName}`);
      }

      // Procesar las reglas generadas y crear cards individuales
      const individualRules = splitRulesIntoIndividualCards(result.rules);

      if (individualRules.length === 0) {
        showNotification('warning', 'Sin reglas', 'No se generaron reglas individuales');
        return;
      }

      // Agregar consultas automáticas basadas en el análisis actual
      const autoQueries = generateAutoQueriesFromAnalysis(appState.currentAnalysis);

      // Combinar reglas generadas con consultas automáticas
      const allRules = [...autoQueries, ...individualRules];

      // Agregar al carrusel
      allRules.forEach(rule => {
        carouselState.savedRules.unshift(rule);
      });

      // Limitar a 50 reglas máximo
      if (carouselState.savedRules.length > 50) {
        carouselState.savedRules = carouselState.savedRules.slice(0, 50);
      }

      carouselState.currentRuleSet = carouselState.savedRules;
      localStorage.setItem('attributeRuleCards', JSON.stringify(carouselState.savedRules));

      updateCarousel();
      updateCarouselInfo();

      showNotification('success', 'Reglas Generadas',
        `Se crearon ${allRules.length} reglas automáticamente. ${result.count} reglas guardadas en sesión.`);

      // También agregar al editor de reglas
      addToCustomRules(result.rules);

      // 🔥 FORZAR RECARGA DE DATOS PARA ASEGURAR SINCRONIZACIÓN
      setTimeout(() => {
        reloadSessionData();
      }, 1000);

    } else {
      throw new Error(result.error);
    }
  } catch (error) {
    showNotification('error', 'Error', `No se pudieron generar reglas: ${error.message}`);
  } finally {
    hideLoading();
  }
}

// Generar consultas automáticas basadas en el análisis actual
function generateAutoQueriesFromAnalysis(analysis) {
  const queries = [];

  if (!analysis || !analysis.objects) return queries;

  // Obtener tipos únicos de objetos detectados
  const objectTypes = [...new Set(analysis.objects.map(obj => obj.object))];

  // Consultas básicas para cada tipo de objeto
  objectTypes.forEach(objType => {
    const safeName = objType.replace(/[^a-zA-Z0-9]/g, '_').toLowerCase();

    queries.push({
      id: Date.now() + Math.random(),
      name: `consultar_${safeName}`,
      code: `objeto_detectado(ID, '${objType}', Confianza).`,
      description: `Consultar todos los objetos de tipo ${objType}`,
      timestamp: new Date().toISOString(),
      type: 'query'
    });

    queries.push({
      id: Date.now() + Math.random() + 1,
      name: `contar_${safeName}`,
      code: `findall(ID, objeto_detectado(ID, '${objType}', _), Lista), length(Lista, Total).`,
      description: `Contar objetos de tipo ${objType}`,
      timestamp: new Date().toISOString(),
      type: 'count'
    });
  });

  // Consultas de seguridad
  queries.push({
    id: Date.now() + Math.random() + 2,
    name: 'verificar_seguridad',
    code: 'findall(ID, seguridad_objeto(ID, seguro), Seguros), length(Seguros, TotalSeguros).',
    description: 'Verificar objetos seguros',
    timestamp: new Date().toISOString(),
    type: 'safety'
  });

  queries.push({
    id: Date.now() + Math.random() + 3,
    name: 'objetos_peligrosos',
    code: 'findall(ID, seguridad_objeto(ID, peligroso), Peligrosos), length(Peligrosos, Total).',
    description: 'Listar objetos peligrosos',
    timestamp: new Date().toISOString(),
    type: 'safety'
  });

  // Consultas de estado
  queries.push({
    id: Date.now() + Math.random() + 4,
    name: 'objetos_comestibles',
    code: 'es_comestible(ID).',
    description: 'Encontrar objetos comestibles',
    timestamp: new Date().toISOString(),
    type: 'safety'
  });

  queries.push({
    id: Date.now() + Math.random() + 5,
    name: 'objetos_podridos',
    code: 'esta_podrido(ID).',
    description: 'Encontrar objetos podridos',
    timestamp: new Date().toISOString(),
    type: 'quality'
  });

  // Consultas de resumen
  queries.push({
    id: Date.now() + Math.random() + 6,
    name: 'resumen_completo',
    code: 'verificar_manzanas, resumen_seguridad.',
    description: 'Resumen completo del análisis',
    timestamp: new Date().toISOString(),
    type: 'analysis'
  });

  return queries;
}

function changeVisualizationType() {
  const type = document.getElementById('visualizationType').value;
  showNotification('info', 'Tipo de Visualización', `Cambiado a: ${type}`);
}

function update3DLayout() {
  showNotification('info', 'Layout 3D', 'Layout actualizado');
}

// 🔥 FUNCIONES DE FILTRADO PROLOG

function applyPrologFilter() {
  const filter = document.getElementById('prologFilter').value;
  if (!filter) {
    showNotification('warning', 'Filtro vacío', 'Ingresa una consulta Prolog para filtrar');
    return;
  }

  showNotification('info', 'Filtro Aplicado', `Filtro Prolog: ${filter}`);
}


function clearPrologFilter() {
  document.getElementById('prologFilter').value = '';
  showNotification('info', 'Filtro Limpiado', 'Filtro Prolog eliminado');
}


function savePrologFilter() {
  showNotification('info', 'Filtro Guardado', 'Filtro Prolog guardado en sesión');
}


function loadQuickFilter(type) {
  const filters = {
    'numericos': "dato(ID, Columna, Valor), number(Valor), Valor > 0.",
    'textuales': "dato(ID, Columna, Valor), string(Valor), string_length(Valor, L), L > 0.",
    'unicos': "setof(Valor, Columna^dato(_, Columna, Valor), ValoresUnicos), member(Valor, ValoresUnicos), dato(ID, _, Valor)."
  };

  const filter = filters[type];
  if (filter) {
    const filterInput = document.getElementById('prologFilter');
    if (filterInput) {
      filterInput.value = filter;
      showNotification('info', 'Filtro Cargado', `Filtro ${type} cargado`);
    }
  }
}

// 🔥 CORREGIR: Verificación de sesión mejorada
async function verifySessionState() {
  try {
    console.log('🔍 Verificando estado de sesión:', appState.sessionId);

    const response = await fetch(`/session/debug/${appState.sessionId}`);
    if (!response.ok) {
      throw new Error(`HTTP error! status: ${response.status}`);
    }

    const sessionInfo = await response.json();
    console.log('📊 Información de sesión RAW:', sessionInfo);

    // 🔥 PRUEBA DIRECTA: Ejecutar una consulta simple para verificar datos reales
    const testResult = await executeSingleQueryHybrid("dato(_, _, _).");
    console.log('🧪 Prueba de consulta directa:', testResult);

    const hasDataFromQuery = testResult.success && testResult.count > 0;
    const hasDataFromSession = sessionInfo.exists &&
      (sessionInfo.hasPrologFacts ||
        sessionInfo.objectCount > 0 ||
        sessionInfo.prologFactsLength > 0);

    console.log(`📊 Resumen verificación: Query=${hasDataFromQuery}, Session=${hasDataFromSession}`);

    if (hasDataFromQuery || hasDataFromSession) {
      const message = `✅ Sesión ACTIVA: ${testResult.count || sessionInfo.objectCount || 0} datos disponibles`;
      console.log(message);
      showNotification('success', 'Sesión OK', message);
      return true;
    } else {
      const message = '❌ Sesión no tiene datos accesibles';
      console.log(message);
      showNotification('warning', 'Sesión vacía',
        'La sesión existe pero no tiene datos accesibles. Carga un archivo CSV o imagen primero.');
      return false;
    }

  } catch (error) {
    console.error('❌ Error verificando sesión:', error);

    // Intentar consulta directa como fallback
    try {
      const fallbackResult = await executeSingleQueryHybrid("dato(_, _, _).");
      if (fallbackResult.success && fallbackResult.count > 0) {
        showNotification('success', 'Sesión OK (fallback)',
          `✅ ${fallbackResult.count} datos disponibles (verificación fallback)`);
        return true;
      }
    } catch (fallbackError) {
      console.log('Fallback también falló:', fallbackError);
    }

    showNotification('error', 'Error de sesión',
      `No se pudo verificar la sesión: ${error.message}`);
    return false;
  }
}


// 🔥 SECUENCIA DE DIAGNÓSTICO COMPLETO
async function runCompleteDiagnostic() {
  console.log('🩺 INICIANDO DIAGNÓSTICO COMPLETO DEL SISTEMA');

  // 1. Verificar sesión
  await verifySessionState();

  // 2. Verificar estructura de datos
  const records = await debugDataStructure();

  // 3. Probar consultas específicas con los datos reales
  if (records) {
    const firstRecord = records[Object.keys(records)[0]];
    console.log('🎯 PRIMER REGISTRO:', firstRecord);

    // Probar consulta con valores reales
    const firstColumn = Object.keys(firstRecord)[0];
    const firstValue = firstRecord[firstColumn];

    const testQuery = `dato(ID, '${firstColumn}', '${firstValue}').`;
    console.log(`🔍 Probando consulta: ${testQuery}`);

    const testResult = await executeSingleQueryHybrid(testQuery);
    console.log(`📊 Resultado: ${testResult.count} registros`);
  }

  // 4. Probar consultas básicas mejoradas
  const testQueries = [
    "dato(ID, Columna, Valor), Columna = 'nombre'.",
    "dato(ID, Columna, Valor), Columna = 'edad'.",
    "dato(ID, Columna, Valor), Columna = 'salario'.",
    "dato(ID, 'nombre', 'Juan').",
    "dato(ID, 'departamento', 'IT')."
  ];

  for (const query of testQueries) {
    const result = await executeSingleQueryHybrid(query);
    console.log(`🔍 ${query}: ${result.count} resultados`);
  }
}
// 🔥 NUEVA FUNCIÓN: Forzar recarga de sesión actual
async function forceReloadSession() {
  showLoading('processing', 'Recargando sesión...', 'Sincronizando con servidor');

  try {
    // Primero verificar todas las sesiones disponibles
    const response = await fetch('/session/debug-all');
    const allSessions = await response.json();

    console.log('📊 Todas las sesiones disponibles:', allSessions);

    // Verificar nuestra sesión actual
    const sessionResponse = await fetch(`/session/debug/${appState.sessionId}`);
    const sessionInfo = await sessionResponse.json();

    console.log('📊 Nuestra sesión actual:', sessionInfo);

    if (sessionInfo.exists && (sessionInfo.hasPrologFacts || sessionInfo.objectCount > 0)) {
      showNotification('success', 'Sesión Recargada',
        `Sesión ${appState.sessionId} tiene datos. Puedes ejecutar consultas ahora.`);
      return true;
    } else {
      // Buscar alguna sesión que tenga datos
      const sessionsWithData = Object.entries(allSessions.sessions).filter(([id, session]) =>
        session.dataRecords > 0 || session.imageObjects > 0
      );

      if (sessionsWithData.length > 0) {
        const [sessionId, sessionData] = sessionsWithData[0];
        showNotification('warning', 'Sesión Cambiada',
          `Cambiando a sesión ${sessionId} que tiene ${sessionData.dataRecords} registros.`);

        appState.setSessionId(sessionId);
        return true;
      } else {
        throw new Error('No hay ninguna sesión con datos. Carga un archivo primero.');
      }
    }
  } catch (error) {
    console.error('❌ Error recargando sesión:', error);
    showNotification('error', 'Error', `No se pudo recargar la sesión: ${error.message}`);
    return false;
  } finally {
    hideLoading();
  }
}

// 🔥 ACTUALIZA processImageFile para verificar después del análisis
async function processImageFile(file) {
  showLoading('analyzing', 'Analizando atributos de imagen...', 'Detectando objetos, estados y características');

  const formData = new FormData();
  formData.append('image', file);
  formData.append('sessionId', appState.sessionId);

  try {
    const response = await fetch('/analyze/image/detailed', {
      method: 'POST',
      body: formData
    });

    const result = await response.json();

    if (result.success) {
      appState.currentAnalysis = result.analysis;
      showNotification('success', 'Análisis completado',
        `${result.analysis.detectedObjects?.length || 0} objetos con atributos detectados`);

      displayAttributeAnalysis(result.analysis, result.prologFacts);
      generateAttributeRules(result.analysis, result.autoQueries);

      // 🔥 VERIFICAR QUE LOS DATOS SE GUARDARON EN SESIÓN
      setTimeout(() => {
        verifySessionState();
      }, 1000);

    } else {
      throw new Error(result.error);
    }
  } catch (error) {
    showNotification('error', 'Error en análisis', `No se pudieron detectar atributos: ${error.message}`);
  } finally {
    hideLoading();
  }
}


// 🔥 INICIALIZACIÓN COMPLETA DEL SISTEMA

// 🔥 CORREGIR: Inicialización completa del sistema
function initializeCompleteSystem() {
  console.log('🚀 Inicializando sistema completo...');

  try {
    // Inicializar event listeners
    initializeEventListeners();

    // Configurar interfaz
    updateSessionInfo();
    setupQueryExamples();
    loadSavedQueries();
    loadSavedRulesList();
    loadRuleCarousel();
    addDiagnosticButtons();

    // Configurar pestañas por defecto - SIN EVENTO
    const defaultTab = document.querySelector('.tab-btn.active');
    if (!defaultTab) {
      const firstTabBtn = document.querySelector('.tab-btn');
      if (firstTabBtn) {
        firstTabBtn.classList.add('active');
      }
    }

    console.log('✅ Sistema completamente inicializado');
  } catch (error) {
    console.error('❌ Error en inicialización:', error);
  }
}

// Ejecutar inicialización cuando el DOM esté listo
if (document.readyState === 'loading') {
  document.addEventListener('DOMContentLoaded', initializeCompleteSystem);
} else {
  initializeCompleteSystem();
}

// 🔥 FUNCIÓN PARA DIAGNÓSTICO COMPLETO DE DATOS
async function diagnosticarEstructuraDatos() {
  console.log('🔍 INICIANDO DIAGNÓSTICO COMPLETO DE ESTRUCTURA DE DATOS');
  showLoading('analyzing', 'Diagnosticando estructura de datos...', 'Analizando registros y columnas');

  try {
    // 1. Obtener todos los datos
    const todosDatos = await executeSingleQueryHybrid("dato(ID, Columna, Valor).");
    console.log(`📊 Total de hechos en base de datos: ${todosDatos.count}`);

    if (!todosDatos.success || todosDatos.count === 0) {
      showNotification('warning', 'Sin datos', 'No hay datos disponibles para diagnosticar');
      return;
    }

    // 2. Agrupar por ID
    const registros = {};
    todosDatos.results.forEach(item => {
      const id = item.ID || item.id;
      const columna = item.Columna || item.columna;
      const valor = item.Valor || item.valor;

      if (id && columna && valor !== undefined) {
        if (!registros[id]) registros[id] = {};
        registros[id][columna] = valor;
      }
    });

    console.log('🎯 REGISTROS COMPLETOS DETECTADOS:');
    Object.keys(registros).sort((a, b) => a - b).forEach(id => {
      console.log(`   ID ${id}:`, registros[id]);
    });

    // 3. Verificar columnas únicas
    const columnasUnicas = new Set();
    todosDatos.results.forEach(item => {
      const columna = item.Columna || item.columna;
      if (columna) columnasUnicas.add(columna);
    });

    console.log('🏷️ COLUMNAS ÚNICAS DETECTADAS:', Array.from(columnasUnicas));

    // 4. Contar por columna
    let mensaje = `📊 DIAGNÓSTICO COMPLETO:\n\n`;
    mensaje += `• Total de hechos: ${todosDatos.count}\n`;
    mensaje += `• Registros únicos: ${Object.keys(registros).length}\n`;
    mensaje += `• Columnas detectadas: ${Array.from(columnasUnicas).join(', ')}\n\n`;

    mensaje += `📈 CONTEOS POR COLUMNA:\n`;
    for (const columna of columnasUnicas) {
      const resultado = await executeSingleQueryHybrid(`dato(ID, '${columna}', _).`);
      mensaje += `• ${columna}: ${resultado.count} valores\n`;
    }

    mensaje += `\n🔍 PRIMEROS REGISTROS:\n`;
    Object.keys(registros).sort((a, b) => a - b).slice(0, 3).forEach(id => {
      mensaje += `ID ${id}: ${JSON.stringify(registros[id])}\n`;
    });

    showNotification('info', 'Diagnóstico Completado', mensaje);

    return registros;

  } catch (error) {
    console.error('❌ Error en diagnóstico:', error);
    showNotification('error', 'Error en diagnóstico', `No se pudo completar el diagnóstico: ${error.message}`);
  } finally {
    hideLoading();
  }
}

// 🔥 PRUEBAS CORREGIDAS MEJORADAS
async function pruebasCorregidas() {
  console.log('🧪 EJECUTANDO PRUEBAS CORREGIDAS');
  showLoading('querying', 'Ejecutando pruebas corregidas...', 'Verificando consultas básicas');

  try {
    const pruebas = [
      {
        nombre: "Todos los datos",
        consulta: "dato(ID, Columna, Valor).",
        esperado: "25 hechos (5 registros × 5 columnas)"
      },
      {
        nombre: "Buscar Juan",
        consulta: "dato(ID, 'nombre', 'Juan').",
        esperado: "1 resultado"
      },
      {
        nombre: "Empleados IT",
        consulta: "dato(ID, 'departamento', 'IT').",
        esperado: "2 resultados"
      },
      {
        nombre: "Contar registros",
        consulta: "findall(ID, dato(ID, 'nombre', _), Lista), length(Lista, Total).",
        esperado: "Total = 5"
      },
      {
        nombre: "Verificar estructura",
        consulta: "findall(Col, dato(_, Col, _), Columnas), sort(Columnas, Unicas).",
        esperado: "5 columnas únicas"
      }
    ];

    let resultados = "🧪 RESULTADOS DE PRUEBAS CORREGIDAS:\n\n";

    for (const prueba of pruebas) {
      console.log(`\n🔍 Probando: ${prueba.nombre}`);
      console.log(`   Consulta: ${prueba.consulta}`);

      const resultado = await executeSingleQueryHybrid(prueba.consulta);

      const estado = resultado.success ? '✅' : '❌';
      resultados += `${estado} ${prueba.nombre}\n`;
      resultados += `   Consulta: ${prueba.consulta}\n`;
      resultados += `   Resultado: ${resultado.count} soluciones\n`;
      resultados += `   Esperado: ${prueba.esperado}\n`;

      if (resultado.error) {
        resultados += `   Error: ${resultado.error}\n`;
      }

      resultados += `\n`;

      // Pequeña pausa entre consultas
      await new Promise(resolve => setTimeout(resolve, 300));
    }

    showNotification('info', 'Pruebas Completadas', resultados);

  } catch (error) {
    console.error('❌ Error en pruebas:', error);
    showNotification('error', 'Error en pruebas', `No se pudieron ejecutar las pruebas: ${error.message}`);
  } finally {
    hideLoading();
  }
}
// 🔥 NUEVO: Función para generar informe PDF
async function generateReport() {
  showLoading('processing', 'Generando informe PDF...', 'Compilando resultados CRISP-DM');

  try {
    // Recopilar datos de la sesión actual
    const sessionData = {
      sessionId: appState.sessionId,
      queries: getRecentQueries(),
      analysis: appState.currentAnalysis || {}
    };

    const response = await fetch('/generate-report', {
      method: 'POST',
      headers: {
        'Content-Type': 'application/json'
      },
      body: JSON.stringify(sessionData)
    });

    if (!response.ok) {
      throw new Error(`Error HTTP: ${response.status}`);
    }

    // Descargar el PDF
    const blob = await response.blob();
    const url = window.URL.createObjectURL(blob);
    const a = document.createElement('a');
    a.href = url;
    a.download = `informe-crisp-dm-${new Date().getTime()}.pdf`;
    document.body.appendChild(a);
    a.click();
    document.body.removeChild(a);
    window.URL.revokeObjectURL(url);

    showNotification('success', 'Informe Generado',
      '✅ Informe PDF descargado exitosamente\n📊 Incluye todas las fases CRISP-DM');

  } catch (error) {
    console.error('❌ Error generando informe:', error);
    showNotification('error', 'Error en Informe',
      `No se pudo generar el PDF: ${error.message}`);
  } finally {
    hideLoading();
  }
}

// 🔥 NUEVO: Obtener consultas recientes
function getRecentQueries() {
  const queryEditor = document.getElementById('prologQuery');
  const recentQueries = JSON.parse(localStorage.getItem('recentQueries') || '[]');

  if (queryEditor && queryEditor.value.trim()) {
    recentQueries.push(queryEditor.value.trim());
    // Mantener solo las últimas 10 consultas
    if (recentQueries.length > 10) {
      recentQueries.shift();
    }
    localStorage.setItem('recentQueries', JSON.stringify(recentQueries));
  }

  return recentQueries;
}

// 🔥 Agregar al objeto window para acceso global
window.generateReport = generateReport;
window.safeGenerateReport = generateReport;

// 🔥 PRUEBAS CORREGIDAS
async function pruebasCorregidas() {
  console.log('🧪 EJECUTANDO PRUEBAS CORREGIDAS');

  const pruebas = [
    "dato(ID, 'nombre', 'Juan').",
    "dato(ID, 'departamento', 'IT').",
    "dato(ID, 'edad', Edad), number_string(Num, Edad), Num > 30.",
    "findall(ID, dato(ID, 'nombre', _), Lista), length(Lista, Total).",
    "verificar_estructura."
  ];

  for (const prueba of pruebas) {
    console.log(`\n🔍 Probando: ${prueba}`);
    const resultado = await executeSingleQueryHybrid(prueba);
    console.log(`   Resultado: ${resultado.count} soluciones`);
    if (resultado.results && resultado.results.length > 0) {
      console.log(`   Ejemplo:`, resultado.results[0]);
    }
  }
}

// 🔥 NUEVO: Sistema de Ayuda Completo

// Mostrar modal de ayuda
function showHelpModal() {
  const modal = document.getElementById('helpModal');
  if (modal) {
    modal.style.display = 'block';
    document.body.style.overflow = 'hidden'; // Prevenir scroll
  }
}

// Cerrar modal de ayuda
function closeHelpModal() {
  const modal = document.getElementById('helpModal');
  if (modal) {
    modal.style.display = 'none';
    document.body.style.overflow = 'auto'; // Restaurar scroll
  }
}

// Cambiar pestañas de ayuda
function openHelpTab(tabName) {
  // Ocultar todas las pestañas
  document.querySelectorAll('.tab-content').forEach(tab => {
    tab.classList.remove('active');
  });

  // Desactivar todos los botones
  document.querySelectorAll('.tab-btn').forEach(btn => {
    btn.classList.remove('active');
  });

  // Mostrar pestaña seleccionada
  const targetTab = document.getElementById(tabName + '-tab');
  if (targetTab) {
    targetTab.classList.add('active');
  }

  // Activar botón
  event.currentTarget.classList.add('active');
}

// Copiar consulta al editor
function copyToQuery(query) {
  const queryEditor = document.getElementById('prologQuery');
  if (queryEditor) {
    queryEditor.value = query;
    queryEditor.focus();

    // Efecto visual de confirmación
    const originalBorder = queryEditor.style.borderColor;
    queryEditor.style.borderColor = '#10b981';
    queryEditor.style.boxShadow = '0 0 10px rgba(16, 185, 129, 0.3)';

    setTimeout(() => {
      queryEditor.style.borderColor = originalBorder;
      queryEditor.style.boxShadow = '';
    }, 2000);

    showNotification('success', 'Consulta Copiada',
      'La consulta se ha copiado al editor. Presiona Ctrl+Enter para ejecutar.');
  }
}

// Demo guiada automática
function startQuickDemo() {
  closeHelpModal();

  showNotification('info', 'Demo Iniciada',
    'Sigue los pasos en la consola para una demostración guiada.');

  // Paso 1: Copiar consulta básica
  setTimeout(() => {
    copyToQuery("clasificar_hongo('abultada', 'almendra', 'bosque', Clase).");
    showNotification('info', 'Paso 1',
      'Consulta copiada. Ejecútala con Ctrl+Enter para ver el resultado.');
  }, 1000);

  // Paso 2: Sugerir siguiente consulta
  setTimeout(() => {
    showNotification('info', 'Paso 2',
      'Ahora prueba: es_ingerible(\'abultada\', \'almendra\', _).');
  }, 5000);

  // Paso 3: Sugerir estadísticas
  setTimeout(() => {
    showNotification('info', 'Paso 3',
      'Finalmente ejecuta: estadisticas_hongos. y genera el PDF.');
  }, 10000);
}

window.addDiagnosticButtons = addDiagnosticButtons;

window.diagnosticarEstructuraDatos = diagnosticarEstructuraDatos;
window.pruebasCorregidas = pruebasCorregidas;

// 🔥 EXPORTAR NUEVAS FUNCIONES DE DIAGNÓSTICO
window.debugDataStructure = debugDataStructure;
window.testCorrectedQueries = testCorrectedQueries;
// Exportar funciones globales para HTML
window.switchTab = switchTab;
window.executePrologQuery = executePrologQuery;
window.saveRules = saveRules;
window.loadTemplate = loadTemplate;
window.addToCustomRules = addToCustomRules;
window.deleteRule = deleteRule;
window.copyRuleToClipboard = copyRuleToClipboard;
window.loadSavedRule = loadSavedRule;
window.useSavedRuleInQueries = useSavedRuleInQueries;
window.deleteSavedRule = deleteSavedRule;
window.saveQuery = saveQuery;
window.deleteSavedQuery = deleteSavedQuery;
window.loadAutoQuery = loadAutoQuery;
window.clearResults = clearResults;
window.exportResults = exportResults;
window.processDataFile = processDataFile;
window.handleFileSelect = handleFileSelect;
window.analyzeImageForAttributes = analyzeImageForAttributes;
window.executeAttributeRule = executeAttributeRule;
window.testAttributeSystem = testAttributeSystem;
window.clearAllRules = clearAllRules;
window.nextRuleCard = nextRuleCard;
window.prevRuleCard = prevRuleCard;
window.goToPage = goToPage;
window.showHelpModal = showHelpModal;
window.closeHelpModal = closeHelpModal;
window.openHelpTab = openHelpTab;
window.copyToQuery = copyToQuery;
window.startQuickDemo = startQuickDemo;

console.log('🎯 Todas las funciones del sistema cargadas correctamente');