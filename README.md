# 🧠 Sistema Prolog + Google Vision AI

**Sistema interactivo didáctico con Prolog**

![Captura del proyecto](https://raw.githubusercontent.com/lazamartinez/Prolog-Interactivo/main/imagenes/Captura1.png)


## 🌟 Características Principales

### 🔍 **Procesamiento Inteligente de Datos**
- **CSV/Excel**: Carga y análisis automático de datos estructurados
- **Consultas Prolog**: Ejecución de consultas complejas en tiempo real
- **Reglas Automáticas**: Generación inteligente de reglas Prolog basadas en datos
- **Persistencia PostgreSQL**: Almacenamiento seguro de sesiones y datos

### 🖼️ **Análisis Avanzado de Imágenes**
- **Google Vision AI**: Detección de objetos, etiquetas y atributos
- **Análisis de Atributos**: Estado, seguridad, calidad de objetos detectados
- **Recomendaciones Automáticas**: Sugerencias basadas en el análisis
- **Generación de Hechos Prolog**: Conversión automática a base de conocimientos

### 🎨 **Interfaz Moderna**
- **Design Glassmorphism**: Interfaz visualmente impactante
- **Responsive Design**: Adaptable a cualquier dispositivo
- **Tiempo Real**: Resultados instantáneos con notificaciones
- **Carrusel de Reglas**: Navegación intuitiva de reglas guardadas

---

## 🚀 Instalación Rápida

### Prerrequisitos
```bash
# Node.js 16+ y PostgreSQL 12+
node --version  # Debe ser 16 o superior
psql --version  # Debe estar instalado
```

### 1. **Clonar y Configurar**
```bash
# Clonar el proyecto
git clone <tu-repositorio>
cd prolog-scraping-app

# Instalar dependencias
npm install

# Configurar entorno
cp .env.example .env
```

### 2. **Configurar PostgreSQL**
Editar `.env`:
```env
PG_USER=postgres
PG_HOST=localhost
PG_DATABASE=prolog_system
PG_PASSWORD=tu_password
PG_PORT=5432
PORT=3000
```

### 3. **Inicializar Base de Datos**
```bash
# Crear base de datos y tablas
node init-database.js
```

### 4. **Ejecutar el Sistema**
```bash
# Modo desarrollo (con auto-recarga)
npm run dev

# Modo producción
npm start
```

### 5. **Acceder al Sistema**
```bash
# Abrir en navegador
http://localhost:3000
```

---

## 🔐 Configuración de Google Cloud Vision API

### **Prerrequisitos para Google Vision**

#### 1. **Crear Proyecto en Google Cloud**
```bash
# Ir a Google Cloud Console
https://console.cloud.google.com/

# Crear nuevo proyecto o seleccionar existente
# Habilitar facturación (primeros $300 son gratis)
```

#### 2. **Habilitar APIs Requeridas**
```bash
# En Google Cloud Console, habilitar:
- Vision API
- Cloud Storage JSON API (opcional)
```

#### 3. **Crear Credenciales de Servicio**
```bash
# En APIs y Servicios > Credenciales
# Click en "Crear Credenciales" > "Cuenta de Servicio"
- Nombre: "prolog-vision-service"
- Rol: "Vision AI Client" o "Viewer"
- Formato de clave: JSON
```

### **Configuración del Archivo de Credenciales**

#### **Ubicación del Archivo**
Coloca el archivo `google-cloud-key.json` en la raíz del proyecto:
```
prolog-scraping-app/
├── google-cloud-key.json  ← AQUÍ
├── server.js
├── package.json
└── ...
```

#### **Estructura del Archivo JSON**
Tu archivo debería verse similar a esto:
```json
{
  "type": "service_account",
  "project_id": "tu-proyecto-id",
  "private_key_id": "abc123...",
  "private_key": "-----BEGIN PRIVATE KEY-----\n...\n-----END PRIVATE KEY-----\n",
  "client_email": "prolog-vision-service@tu-proyecto-id.iam.gserviceaccount.com",
  "client_id": "1234567890",
  "auth_uri": "https://accounts.google.com/o/oauth2/auth",
  "token_uri": "https://oauth2.googleapis.com/token",
  "auth_provider_x509_cert_url": "https://www.googleapis.com/oauth2/v1/certs",
  "client_x509_cert_url": "https://www.googleapis.com/robot/v1/metadata/x509/prolog-vision-service%40tu-proyecto-id.iam.gserviceaccount.com"
}
```

### **Configuración Alternativa**

#### **Opción A: Variable de Entorno**
```bash
# En lugar del archivo JSON, puedes usar variable de entorno
export GOOGLE_APPLICATION_CREDENTIALS="path/to/your/google-cloud-key.json"

# O incluir directamente en .env:
GOOGLE_CLOUD_CREDENTIALS='{"type": "service_account", "project_id": "...", ...}'
```

#### **Opción B: Sin Google Vision**
El sistema funciona perfectamente sin Google Vision, usando análisis local básico.

### **Verificar Configuración**

#### **Prueba de Conexión**
```bash
# Instalar Google Cloud SDK (opcional)
curl https://sdk.cloud.google.com | bash
exec -l $SHELL
gcloud auth activate-service-account --key-file=google-cloud-key.json

# Probar Vision API
gcloud ml vision detect-labels ./test-image.jpg
```

#### **Verificación en el Sistema**
1. **Inicia el servidor:**
   ```bash
   npm run dev
   ```

2. **Revisa los logs:**
   ```
   ✅ Google Cloud Vision API configurado correctamente
   ```

3. **Prueba con una imagen:**
   - Sube cualquier imagen en la pestaña "Imágenes"
   - Deberías ver análisis detallado de Google Vision

---

## 📊 Flujo de Trabajo

### 🗃️ **Para Datos CSV/Excel**

1. **Cargar Archivo**
   - Ve a la pestaña "Datos"
   - Arrastra o selecciona tu archivo CSV/Excel
   - El sistema procesa automáticamente y genera hechos Prolog

2. **Ejecutar Consultas**
   ```prolog
   -- Consultas básicas
   dato(ID, Columna, Valor).
   dato(ID, 'nombre', 'Juan').
   dato(ID, 'departamento', 'IT').

   -- Consultas avanzadas
   dato(ID, 'salario', Salario), Salario @> '35000'.
   findall(ID, dato(ID, 'departamento', 'Ventas'), Ventas), length(Ventas, Total).
   ```

3. **Generar Reglas Automáticas**
   - Usa el botón "Generar Automáticamente"
   - El sistema crea reglas basadas en tu estructura de datos

### 🖼️ **Para Análisis de Imágenes**

1. **Subir Imagen**
   - Ve a la pestaña "Imágenes"
   - Selecciona una imagen (JPG, PNG, GIF)
   - El sistema analiza con Google Vision AI

2. **Ver Resultados**
   - Objetos detectados con confianza
   - Atributos y estados automáticos
   - Recomendaciones de seguridad
   - Hechos Prolog generados automáticamente

3. **Consultas de Imágenes**
   ```prolog
   objeto_detectado(ID, Objeto, Confianza).
   es_comestible(ID).
   esta_podrido(ID).
   resumen_seguridad.
   ```

---

## 🛠️ Funcionalidades Avanzadas

### 🔧 **Herramientas de Diagnóstico y Limpieza**
```javascript
// En la interfaz, sección de consultas y navbar:
- "Verificar Sesión": Estado actual de la sesión
- "Estado BD": Estadísticas completas de la base de datos
- "Limpiar Sesión": Elimina datos de la sesión actual
- "Limpiar BD": ⚠️ Elimina TODA la base de datos (disponible en navbar)
```

### 📚 **Sistema de Reglas**
- **Editor Integrado**: Escribe y guarda reglas personalizadas
- **Plantillas**: Filtros, clasificación, cálculos, validación
- **Carrusel Interactivo**: Navegación visual de reglas guardadas
- **Ejecución Directa**: Prueba reglas con un clic

### 🎯 **Consultas Predefinidas**
El sistema incluye ejemplos para:
- Análisis de objetos detectados
- Verificación de seguridad
- Conteos y estadísticas
- Filtros por atributos

---

## 🗃️ Estructura de Base de Datos

### **Tablas Principales**
```sql
sessions           -- Gestión de sesiones de usuario
prolog_facts       -- Hechos Prolog (datos e imágenes)
prolog_rules       -- Reglas personalizadas guardadas
saved_queries      -- Consultas favoritas
```

### **Ejemplo de Hechos Generados**
```prolog
% Desde CSV
dato(1, 'nombre', 'Juan').
dato(1, 'edad', '25').
dato(1, 'ciudad', 'Madrid').

% Desde Imágenes
objeto_detectado(1, 'manzana', '85%').
seguridad_objeto(1, 'seguro').
estado_objeto(1, 'en_buen_estado').
```

---

## 🎨 Personalización

### **Agregar Nuevos Tipos de Análisis**
```javascript
// En server.js, clase AdvancedImageAnalysis
generateEnhancedAttributes(objectName, originalName) {
    // Agregar lógica personalizada aquí
    if (objectName.includes('tu_objeto')) {
        return ['nuevo_atributo', 'personalizado'];
    }
}
```

### **Crear Nuevas Plantillas de Reglas**
```javascript
// En app.js, función loadTemplate()
const templates = {
    custom_analysis: `
        % Tu análisis personalizado
        mi_analisis(X) :- 
            dato(X, Columna, Valor),
            condicion_personalizada(Valor).
    `
};
```

### **Personalizar Funciones de Vision**
```javascript
// En server.js, clase ComputerVisionAPI
async analyzeWithGoogleVision(imageBuffer) {
  const request = {
    image: { content: imageBuffer },
    features: [
      { type: 'LABEL_DETECTION', maxResults: 20 },
      { type: 'OBJECT_LOCALIZATION', maxResults: 15 },
      { type: 'FACE_DETECTION', maxResults: 10 },        // ← Agregar
      { type: 'TEXT_DETECTION', maxResults: 10 },        // ← Agregar
      { type: 'LOGO_DETECTION', maxResults: 5 },         // ← Agregar
      { type: 'SAFE_SEARCH_DETECTION' },
      { type: 'IMAGE_PROPERTIES' }
    ]
  };
  // ... resto del código
}
```

### **Traducciones Personalizadas**
```javascript
translateObjectToSpanish(objectName) {
  const translations = {
    // Agregar más traducciones
    "mushroom": "hongo",
    "flower": "flor",
    "tree": "árbol",
    "car": "coche",
    "building": "edificio",
    // ... tus traducciones personalizadas
  };
  return translations[objectName] || objectName;
}
```

---

## 🚨 Solución de Problemas

### **Error: "No se puede conectar a PostgreSQL"**
```bash
# Verificar que PostgreSQL esté ejecutándose
sudo service postgresql start

# Verificar credenciales en .env
psql -U postgres -h localhost

# Recrear base de datos
node init-database.js
```

### **Error: "Google Vision no disponible"**
```bash
# El sistema funciona sin Google Vision
# Usará análisis local básico
# Para habilitar Google Vision:
# 1. Crear cuenta en Google Cloud
# 2. Habilitar Vision API
# 3. Descargar credenciales a google-cloud-key.json
```

### **Error: "No se encontraron credenciales" de Google Vision**
```bash
# Verificar que el archivo existe
ls -la google-cloud-key.json

# Verificar permisos
chmod 600 google-cloud-key.json

# Verificar contenido
cat google-cloud-key.json | jq .project_id
```

### **Error: "Permission Denied" en Google Vision**
```bash
# Verificar que la cuenta de servicio tenga permisos
# En Google Cloud Console > IAM > Administrar
# Buscar el email de la cuenta de servicio
# Asignar rol: "Vision AI Client"
```

### **Error: "Billing not enabled"**
```bash
# Habilitar facturación en Google Cloud Console
# Los primeros $300 son gratis por 90 días
```

### **Limpiar Base de Datos Completamente**
```bash
# Opción 1: Desde la interfaz
# Click en "Limpiar BD" en el navbar

# Opción 2: Desde consola
node -e "
const { Pool } = require('pg');
const pool = new Pool();
async function clean() {
    await pool.query('DELETE FROM prolog_facts');
    await pool.query('DELETE FROM prolog_rules');
    await pool.query('DELETE FROM saved_queries');
    await pool.query('DELETE FROM sessions');
    console.log('✅ Base de datos limpiada');
    process.exit();
}
clean();
"
```

---

## 📈 Ejemplos de Uso

### **Caso 1: Análisis de Empleados**
```csv
nombre,edad,ciudad,salario,departamento
Juan,25,Madrid,30000,Ventas
Maria,30,Barcelona,35000,IT
```

**Consultas útiles:**
```prolog
% Empleados por departamento
findall(Nombre, (dato(ID, 'nombre', Nombre), dato(ID, 'departamento', 'IT')), IT).

% Salario promedio
findall(Salario, dato(_, 'salario', Salario), Salarios), 
promedio(Salarios, Promedio).
```

### **Caso 2: Clasificación de Imágenes**
**El sistema detecta automáticamente:**
- 🍎 **Manzana**: "comestible", "en_buen_estado", "seguro"
- 🍌 **Plátano**: "madura", "comestible", "amarillo"
- 🍄 **Hongo**: "peligroso", "no_comestible", "verificar"

### **Ejemplo de Análisis Exitoso con Google Vision**
```
🔍 Iniciando análisis de imagen...
🔄 Intentando Google Cloud Vision API...
✅ Google Cloud Vision API - Análisis exitoso
📦 Objetos detectados: 3
   - apple -> manzana (85.5%)
   - banana -> plátano (92.1%)
   - fruit -> fruta (78.3%)
🎨 Mejorando análisis con atributos adicionales...
✅ Análisis completado: 3 objetos, 15 etiquetas
```

---

## 📊 Monitoreo y Logs

### **Ver Uso en Google Cloud**
```bash
# En Google Cloud Console:
- Ir a "APIs y Servicios" > "Dashboard"
- Ver métricas de Vision API
- Revisar logs en "Logging"
```

### **Logs del Sistema**
El sistema registra automáticamente:
- ✅ Conexión exitosa a Google Vision
- 🔍 Objetos detectados y confianza
- ⚠️ Fallbacks a análisis local
- 📊 Estadísticas de uso

### **Límites y Costos de Google Vision**
```bash
# Precios (pueden cambiar):
- Primeros 1000 unidades/mes: GRATIS
- 1001-5,000,000: $1.50 por 1000 unidades
- 1 unidad = 1 imagen analizada

# Límites por defecto:
- 1800 requests por minuto
- 600 requests por minuto por usuario
```

---

## 🔒 Seguridad

### **Protección de Credenciales**
```bash
# NUNCA commits las credenciales
echo "google-cloud-key.json" >> .gitignore
echo "*.json" >> .gitignore

# Usar variables de entorno en producción
```

### **Rotación de Claves**
```bash
# Rotar claves cada 90 días
# En Google Cloud Console > APIs y Servicios > Credenciales
# Generar nueva clave JSON
# Actualizar archivo o variable de entorno
```

---

## 🔮 Próximas Características

- [ ] **Integración con más APIs de ML** (Azure Cognitive Services, AWS Rekognition)
- [ ] **Análisis de video en tiempo real**
- [ ] **Exportación a múltiples formatos** (JSON, XML, PDF)
- [ ] **Sistema de plugins para reglas personalizadas**
- [ ] **APIs REST para integración externa**

---

## 👥 Desarrollo

**Desarrollado por:**
- Küster Joaquín
- Da Silva Marcos  
- Martinez Lázaro Ezequiel

**Universidad Nacional de Misiones - FCEQyN**  
**Paradigmas y Lenguajes de Programación 2025**

---

## 📄 Licencia

Este proyecto es para fines educativos y de investigación. Desarrollado como parte del curso de Paradigmas y Lenguajes de Programación.

---

**¡Listo para comenzar! 🚀**

El sistema está diseñado para ser intuitivo y potente. Comienza cargando un archivo CSV o imagen y explora las capacidades de Prolog combinadas con Machine Learning moderno.

¿Necesitas ayuda? Revisa la sección de solución de problemas o ejecuta las herramientas de diagnóstico integradas.
