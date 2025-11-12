const { Client } = require('pg');
require('dotenv').config();

async function initializeDatabase() {
  const client = new Client({
    user: process.env.PG_USER,
    host: process.env.PG_HOST,
    database: 'postgres', // Conectarse a la BD por defecto primero
    password: process.env.PG_PASSWORD,
    port: process.env.PG_PORT,
  });

  try {
    await client.connect();
    console.log('🔗 Conectado a PostgreSQL...');

    // Crear la base de datos si no existe
    const dbName = process.env.PG_DATABASE || 'prolog_system';
    const result = await client.query(
      `SELECT 1 FROM pg_database WHERE datname = $1`,
      [dbName]
    );

    if (result.rows.length === 0) {
      console.log(`📁 Creando base de datos: ${dbName}`);
      await client.query(`CREATE DATABASE ${dbName}`);
      console.log('✅ Base de datos creada');
    } else {
      console.log(`✅ Base de datos ${dbName} ya existe`);
    }

    await client.end();

    // Ahora conectarse a la base de datos específica para crear las tablas
    const dbClient = new Client({
      user: process.env.PG_USER,
      host: process.env.PG_HOST,
      database: dbName,
      password: process.env.PG_PASSWORD,
      port: process.env.PG_PORT,
    });

    await dbClient.connect();

    // Crear tablas
    console.log('📊 Creando tablas...');

    // Tabla de sesiones
    await dbClient.query(`
      CREATE TABLE IF NOT EXISTS sessions (
        id VARCHAR(100) PRIMARY KEY,
        created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
        updated_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
        metadata JSONB
      )
    `);

    // Tabla de hechos Prolog
    await dbClient.query(`
      CREATE TABLE IF NOT EXISTS prolog_facts (
        id SERIAL PRIMARY KEY,
        session_id VARCHAR(100) REFERENCES sessions(id) ON DELETE CASCADE,
        fact_type VARCHAR(50) NOT NULL,
        predicate VARCHAR(100) NOT NULL,
        arguments JSONB NOT NULL,
        created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP
      )
    `);

    // Tabla de reglas Prolog
    await dbClient.query(`
      CREATE TABLE IF NOT EXISTS prolog_rules (
        id SERIAL PRIMARY KEY,
        session_id VARCHAR(100) REFERENCES sessions(id) ON DELETE CASCADE,
        rule_name VARCHAR(100) NOT NULL,
        rule_code TEXT NOT NULL,
        rule_type VARCHAR(50),
        created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
        UNIQUE(session_id, rule_name)
      )
    `);

    // Tabla de consultas guardadas
    await dbClient.query(`
      CREATE TABLE IF NOT EXISTS saved_queries (
        id SERIAL PRIMARY KEY,
        session_id VARCHAR(100) REFERENCES sessions(id) ON DELETE CASCADE,
        query_name VARCHAR(100) NOT NULL,
        query_code TEXT NOT NULL,
        created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP
      )
    `);

    console.log('✅ Tablas creadas exitosamente');
    await dbClient.end();

  } catch (error) {
    console.error('❌ Error inicializando la base de datos:', error.message);
    process.exit(1);
  }
}

// Ejecutar si se llama directamente
if (require.main === module) {
  initializeDatabase().then(() => {
    console.log('🎉 Inicialización completada');
    process.exit(0);
  });
}

module.exports = { initializeDatabase };