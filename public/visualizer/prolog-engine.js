class PrologEngine {
    constructor() {
        this.session = pl.create();
        this.program = '';
        this.facts = [];
        this.rules = [];
        this.predicates = new Map();
        this.predicateIndex = new Map();
        this.nodeCounter = 0;
    }

    async loadProgram(programText) {
        return new Promise((resolve, reject) => {
            this.session.consult(programText, {
                success: () => {
                    this.program = programText;
                    this.analyzeProgram();
                    resolve(true);
                },
                error: (err) => {
                    reject(new Error(`Error cargando programa: ${err}`));
                }
            });
        });
    }

    analyzeProgram() {
        this.facts = [];
        this.rules = [];
        this.predicates.clear();
        this.predicateIndex.clear();
        this.nodeCounter = 0;

        const rawLines = this.program.split('\n')
            .map(line => line.trim())
            .filter(line => line && !line.startsWith('%'));

        console.log('=== PROCESANDO LÍNEAS CRUDAS ===');
        console.log('Líneas crudas:', rawLines);

        // Unir líneas continuas
        const lines = this.joinContinuedLines(rawLines);

        console.log('=== LÍNEAS UNIDAS ===');
        console.log('Líneas unidas:', lines);

        console.log('=== ANALIZANDO LÍNEAS DEL PROGRAMA ===');
        lines.forEach((line, index) => {
            console.log(`Línea ${index}: "${line}"`);

            try {
                if (line.includes(':-')) {
                    console.log(`📝 Identificada como REGLA: ${line}`);
                    const rule = this.parseRule(line);
                    if (rule) {
                        this.rules.push(rule);
                        this.registerPredicate(rule.head.predicate, 'rule', rule.head.arguments.length);
                        console.log(`✅ Regla parseada correctamente`);
                    } else {
                        console.log(`❌ Error parseando regla`);
                    }
                } else if (line.endsWith(').')) {
                    console.log(`📝 Identificada como HECHO con args: ${line}`);
                    const fact = this.parseFact(line);
                    if (fact) {
                        this.facts.push(fact);
                        this.registerPredicate(fact.predicate, 'fact', fact.arguments.length);
                        console.log(`✅ Hecho parseado correctamente`);
                    }
                } else if (line.endsWith('.')) {
                    console.log(`📝 Identificada como HECHO simple: ${line}`);
                    const predicate = line.replace('.', '').trim();
                    if (predicate && !predicate.includes('(') && !predicate.includes(')')) {
                        const fact = {
                            type: 'fact',
                            predicate: predicate,
                            arguments: [],
                            original: line,
                            arity: 0
                        };
                        this.facts.push(fact);
                        this.registerPredicate(predicate, 'fact', 0);
                        console.log(`✅ Hecho simple parseado correctamente`);
                    }
                }
            } catch (error) {
                console.warn(`❌ Error analizando línea ${index}:`, line, error);
            }
        });

        console.log('Análisis completado:', {
            facts: this.facts.length,
            rules: this.rules.length,
            predicates: this.predicates.size
        });
    }

    joinContinuedLines(rawLines) {
        const lines = [];
        let currentLine = '';

        for (let i = 0; i < rawLines.length; i++) {
            const line = rawLines[i];

            // Si la línea actual está vacía, empezar una nueva
            if (!currentLine) {
                currentLine = line;
            } else {
                // Unir con la línea anterior
                currentLine += ' ' + line;
            }

            // Si la línea termina con punto, es una línea completa
            if (currentLine.endsWith('.')) {
                lines.push(currentLine);
                currentLine = '';
            }
        }

        // Si queda alguna línea sin terminar, agregarla (para manejar programas incompletos)
        if (currentLine) {
            lines.push(currentLine);
        }

        return lines;
    }

    parseFact(factLine) {
        const cleanLine = factLine.replace(/\.$/, '');

        const match = cleanLine.match(/^([a-z][a-zA-Z0-9_]*)\s*\((.*)\)$/);
        if (match) {
            const predicate = match[1];
            const argsStr = match[2];
            const args = this.parseArguments(argsStr);

            return {
                type: 'fact',
                predicate: predicate,
                arguments: args,
                original: factLine,
                arity: args.length
            };
        } else {
            return {
                type: 'fact',
                predicate: cleanLine,
                arguments: [],
                original: factLine,
                arity: 0
            };
        }
    }

    parseRule(ruleLine) {
        console.log(`Parseando regla: "${ruleLine}"`);

        const cleanLine = ruleLine.replace(/\.$/, '');
        const parts = cleanLine.split(':-');

        if (parts.length !== 2) {
            console.log(`❌ No se pudo dividir la regla: ${ruleLine}`);
            return null;
        }

        const head = parts[0].trim();
        const body = parts[1].trim();

        console.log(`Cabeza: "${head}", Cuerpo: "${body}"`);

        // Parsear cabeza de la regla
        const headMatch = head.match(/^([a-z][a-zA-Z0-9_]*)\s*\((.*)\)$/);
        if (!headMatch) {
            console.log(`❌ No se pudo parsear cabeza: ${head}`);
            return null;
        }

        const headPredicate = headMatch[1];
        const headArgsStr = headMatch[2];
        const headArgs = this.parseArguments(headArgsStr);

        console.log(`Cabeza parseada: ${headPredicate} con ${headArgs.length} argumentos`);

        // Parsear cuerpo de la regla
        const bodyGoals = this.parseBody(body);

        console.log(`Cuerpo parseado: ${bodyGoals.length} goals`, bodyGoals);

        return {
            type: 'rule',
            head: {
                predicate: headPredicate,
                arguments: headArgs,
                arity: headArgs.length
            },
            body: bodyGoals,
            original: ruleLine,
            arity: headArgs.length
        };
    }

    parseBody(body) {
        console.log(`Parseando cuerpo: "${body}"`);

        if (!body || body.trim() === '') {
            console.log('❌ Cuerpo vacío');
            return [];
        }

        const goals = [];
        let current = '';
        let depth = 0;
        let inQuotes = false;
        let quoteChar = '';

        // Manejar ORs (;) y ANDs (,)
        for (let i = 0; i < body.length; i++) {
            const char = body[i];

            if ((char === '"' || char === "'") && !inQuotes) {
                inQuotes = true;
                quoteChar = char;
                current += char;
            } else if (char === quoteChar && inQuotes) {
                inQuotes = false;
                current += char;
            } else if (char === '(' && !inQuotes) {
                depth++;
                current += char;
            } else if (char === ')' && !inQuotes) {
                depth--;
                current += char;
            } else if ((char === ',' || char === ';') && depth === 0 && !inQuotes) {
                if (current.trim()) {
                    console.log(`Goal encontrado: "${current.trim()}"`);
                    const goal = this.parseGoal(current.trim());
                    if (goal) {
                        goals.push(goal);
                    }
                }
                current = '';
            } else {
                current += char;
            }
        }

        // Procesar el último goal
        if (current.trim()) {
            console.log(`Último goal: "${current.trim()}"`);
            const goal = this.parseGoal(current.trim());
            if (goal) {
                goals.push(goal);
            }
        }

        console.log(`Total de goals parseados: ${goals.length}`);
        return goals;
    }

    parseGoal(goalStr) {
        console.log(`Parseando goal: "${goalStr}"`);
        goalStr = goalStr.trim();

        if (goalStr.includes('=')) {
            const parts = goalStr.split('=').map(part => part.trim());
            if (parts.length === 2) {
                console.log(`Goal de unificación: ${parts[0]} = ${parts[1]}`);
                return {
                    predicate: '=',
                    arguments: [
                        this.parseArgument(parts[0]),
                        this.parseArgument(parts[1])
                    ],
                    isUnification: true,
                    arity: 2
                };
            }
        }

        // Manejar predicados normales
        const match = goalStr.match(/^([a-z][a-zA-Z0-9_]*)\s*\((.*)\)$/);
        if (match) {
            const predicate = match[1];
            const argsStr = match[2];
            const args = this.parseArguments(argsStr);

            console.log(`Goal de predicado: ${predicate} con ${args.length} argumentos`);

            return {
                predicate: predicate,
                arguments: args,
                arity: args.length
            };
        } else {
            // Predicado sin argumentos
            console.log(`Goal sin argumentos: ${goalStr}`);
            return {
                predicate: goalStr,
                arguments: [],
                arity: 0
            };
        }
    }

    parseArguments(argsStr) {
        if (!argsStr.trim()) return [];

        const args = [];
        let current = '';
        let depth = 0;
        let inQuotes = false;
        let quoteChar = '';

        for (let char of argsStr) {
            if ((char === '"' || char === "'") && !inQuotes) {
                inQuotes = true;
                quoteChar = char;
                current += char;
            } else if (char === quoteChar && inQuotes) {
                inQuotes = false;
                current += char;
            } else if (char === '(' && !inQuotes) {
                depth++;
                current += char;
            } else if (char === ')' && !inQuotes) {
                depth--;
                current += char;
            } else if (char === ',' && depth === 0 && !inQuotes) {
                args.push(current.trim());
                current = '';
            } else {
                current += char;
            }
        }

        if (current.trim()) {
            args.push(current.trim());
        }

        return args.map(arg => this.parseArgument(arg));
    }

    parseArgument(arg) {
        arg = arg.trim();

        if (arg.match(/^[A-Z_][a-zA-Z0-9_]*$/)) {
            return { type: 'variable', value: arg };
        }
        else if (arg.match(/^-?\d+\.?\d*$/)) {
            return { type: 'number', value: parseFloat(arg) };
        }
        else if ((arg.startsWith("'") && arg.endsWith("'")) ||
            (arg.startsWith('"') && arg.endsWith('"'))) {
            return { type: 'string', value: arg.slice(1, -1) };
        }
        else if (arg.match(/^[a-z][a-zA-Z0-9_]*$/)) {
            return { type: 'atom', value: arg };
        }
        else if (arg === '_') {
            return { type: 'variable', value: '_' };
        }
        else {
            return { type: 'compound', value: arg };
        }
    }

    registerPredicate(predicate, type, arity) {
        const key = `${predicate}/${arity}`;

        if (!this.predicates.has(predicate)) {
            this.predicates.set(predicate, {
                type: type,
                count: 0,
                usages: [],
                arities: new Set()
            });
        }

        const predInfo = this.predicates.get(predicate);
        predInfo.count++;
        predInfo.arities.add(arity);

        if (!this.predicateIndex.has(key)) {
            this.predicateIndex.set(key, {
                rules: [],
                facts: [],
                goals: []
            });
        }
    }

    buildPredicateIndex() {
        this.predicateIndex.clear();

        this.rules.forEach((rule, index) => {
            const key = `${rule.head.predicate}/${rule.head.arity}`;
            if (!this.predicateIndex.has(key)) {
                this.predicateIndex.set(key, { rules: [], facts: [], goals: [] });
            }
            this.predicateIndex.get(key).rules.push({
                id: `rule_${index}`,
                rule: rule,
                index: index
            });
        });

        this.facts.forEach((fact, index) => {
            const key = `${fact.predicate}/${fact.arity}`;
            if (!this.predicateIndex.has(key)) {
                this.predicateIndex.set(key, { rules: [], facts: [], goals: [] });
            }
            this.predicateIndex.get(key).facts.push({
                id: `fact_${index}`,
                fact: fact,
                index: index
            });
        });

        return this.predicateIndex;
    }

    generateRuleTree() {
        console.log('Generando árbol de reglas...');

        const tree = {
            nodes: [],
            connections: []
        };

        this.buildPredicateIndex();

        // Crear nodos de reglas
        this.rules.forEach((rule, index) => {
            const ruleId = `rule_${index}`;
            const ruleLabel = this.formatPredicateLabel(rule.head.predicate, rule.head.arguments);

            tree.nodes.push({
                id: ruleId,
                type: 'rule',
                label: ruleLabel,
                x: -300,
                y: index * 150 - (this.rules.length * 150) / 2,
                original: rule.original,
                prologData: rule,
                predicate: rule.head.predicate,
                arity: rule.head.arity,
                level: 0
            });

            // DEBUG: Mostrar información de la regla
            console.log(`Regla ${index}: ${ruleLabel}`, {
                bodyLength: rule.body.length,
                body: rule.body
            });

            // Crear nodos de goals
            rule.body.forEach((goal, goalIndex) => {
                const goalId = `goal_${index}_${goalIndex}`;
                const goalLabel = goal.isUnification ?
                    `${goal.arguments[0].value} = ${goal.arguments[1].value}` :
                    this.formatPredicateLabel(goal.predicate, goal.arguments);

                const goalType = goal.predicate === '=' ? 'variable' : 'predicate';

                const goalNode = {
                    id: goalId,
                    type: goalType,
                    label: goalLabel,
                    x: 0,
                    y: index * 150 - (this.rules.length * 150) / 2 + (goalIndex - (rule.body.length - 1) / 2) * 80,
                    original: goalLabel,
                    prologData: goal,
                    predicate: goal.predicate,
                    arity: goal.arity,
                    level: 1,
                    parentRule: ruleId
                };

                tree.nodes.push(goalNode);

                // DEBUG: Mostrar información del goal
                console.log(`  Goal ${goalIndex}:`, {
                    id: goalId,
                    predicate: goal.predicate,
                    arity: goal.arity,
                    type: goalType,
                    label: goalLabel
                });

                // Conexión regla -> goal
                tree.connections.push({
                    id: `conn_${ruleId}_${goalId}`,
                    from: ruleId,
                    to: goalId,
                    type: 'contains',
                    meta: {
                        predicate: goal.predicate,
                        arity: goal.arity,
                        connectionType: 'rule_to_goal'
                    }
                });
            });
        });

        // Crear nodos de hechos
        this.facts.forEach((fact, index) => {
            const factId = `fact_${index}`;
            const factLabel = this.formatPredicateLabel(fact.predicate, fact.arguments);

            tree.nodes.push({
                id: factId,
                type: 'fact',
                label: factLabel,
                x: 300,
                y: index * 100 - (this.facts.length * 100) / 2,
                original: fact.original,
                prologData: fact,
                predicate: fact.predicate,
                arity: fact.arity,
                level: 2
            });
        });

        // DEBUG: Mostrar todos los nodos antes de crear conexiones
        console.log('Nodos creados:', tree.nodes.map(n => ({
            id: n.id,
            type: n.type,
            predicate: n.predicate,
            arity: n.arity
        })));

        // Crear conexiones de goals a reglas/hechos
        this.createGoalConnections(tree);

        console.log('Árbol generado:', {
            nodes: tree.nodes.length,
            connections: tree.connections.length
        });

        return tree;
    }

    createGoalConnections(tree) {
        console.log('=== CREANDO CONEXIONES DE GOALS ===');

        // Buscar TODOS los nodos que son goals (predicates o variables)
        const goalNodes = tree.nodes.filter(node => {
            const isGoal = (node.type === 'predicate' || node.type === 'variable') &&
                node.prologData &&
                node.predicate &&
                node.predicate !== '=';

            if (isGoal) {
                console.log(`Goal encontrado: ${node.id} - ${node.label} (${node.predicate}/${node.arity})`);
            }

            return isGoal;
        });

        console.log(`Encontrados ${goalNodes.length} goals para conectar:`,
            goalNodes.map(g => `${g.id}: ${g.label}`));

        goalNodes.forEach(goalNode => {
            const goalPredicate = goalNode.predicate;
            const goalArity = goalNode.arity;
            const exactKey = `${goalPredicate}/${goalArity}`;

            console.log(`\nProcesando goal: ${goalNode.label} (${exactKey})`);

            const matches = this.predicateIndex.get(exactKey);
            if (matches) {
                console.log(`✅ Coincidencias EXACTAS encontradas para ${exactKey}:`, {
                    rules: matches.rules.length,
                    facts: matches.facts.length
                });

                // Conectar con reglas
                matches.rules.forEach(ruleMatch => {
                    // Verificar que no sea autoconexión
                    const isSelfConnection = goalNode.parentRule === ruleMatch.id;

                    if (!isSelfConnection) {
                        console.log(`  🔗 Conectando goal ${goalNode.id} -> rule ${ruleMatch.id}`);
                        tree.connections.push({
                            id: `conn_${goalNode.id}_${ruleMatch.id}`,
                            from: goalNode.id,
                            to: ruleMatch.id,
                            type: 'resolves_with_rule',
                            meta: {
                                predicate: goalPredicate,
                                arity: goalArity,
                                connectionType: 'goal_to_rule',
                                matchType: 'exact'
                            }
                        });
                    } else {
                        console.log(`  ⚠️ Evitando autoconexión: ${goalNode.id} -> ${ruleMatch.id}`);
                    }
                });

                // Conectar con hechos
                matches.facts.forEach(factMatch => {
                    console.log(`  🔗 Conectando goal ${goalNode.id} -> fact ${factMatch.id}`);
                    tree.connections.push({
                        id: `conn_${goalNode.id}_${factMatch.id}`,
                        from: goalNode.id,
                        to: factMatch.id,
                        type: 'resolves_with_fact',
                        meta: {
                            predicate: goalPredicate,
                            arity: goalArity,
                            connectionType: 'goal_to_fact',
                            matchType: 'exact'
                        }
                    });
                });
            } else {
                console.log(`❌ No se encontraron coincidencias EXACTAS para: ${exactKey}`);

                // Buscar coincidencias parciales (mismo predicate diferente arity)
                let foundPartial = false;
                for (let [key, value] of this.predicateIndex) {
                    const [pred, arr] = key.split('/');
                    const parsedArity = parseInt(arr);

                    if (pred === goalPredicate && parsedArity !== goalArity) {
                        console.log(`🟡 Coincidencia PARCIAL encontrada: ${key} para goal ${goalPredicate}/${goalArity}`);
                        foundPartial = true;

                        // Conectar con reglas de mismo predicate diferente arity
                        value.rules.forEach(ruleMatch => {
                            if (goalNode.parentRule !== ruleMatch.id) {
                                tree.connections.push({
                                    id: `conn_${goalNode.id}_${ruleMatch.id}_partial`,
                                    from: goalNode.id,
                                    to: ruleMatch.id,
                                    type: 'resolves_with_rule',
                                    meta: {
                                        predicate: goalPredicate,
                                        arity: goalArity,
                                        targetArity: parsedArity,
                                        connectionType: 'goal_to_rule',
                                        matchType: 'same_predicate_different_arity'
                                    }
                                });
                            }
                        });
                    }
                }

                if (!foundPartial) {
                    console.log(`🔴 No se encontraron coincidencias PARCIALES para: ${goalPredicate}`);
                }
            }
        });

        console.log(`=== TOTAL DE CONEXIONES CREADAS: ${tree.connections.length} ===`);
    }

    formatPredicateLabel(predicate, argsList) {
        const args = argsList.map(arg => {
            if (arg.type === 'variable') return arg.value;
            if (arg.type === 'string') return `'${arg.value}'`;
            if (arg.type === 'atom') return arg.value;
            if (arg.type === 'number') return arg.value.toString();
            return arg.value;
        }).join(', ');

        return args ? `${predicate}(${args})` : predicate;
    }

    async executeQuery(query, stepCallback = null) {
        return new Promise((resolve, reject) => {
            const results = [];
            let stepCount = 0;

            this.session.query(query, {
                success: (goal) => {
                    const findSolutions = () => {
                        this.session.answer({
                            success: (answer) => {
                                stepCount++;

                                const result = {
                                    answer: pl.format_answer(answer),
                                    variables: this.extractVariables(answer),
                                    step: stepCount
                                };

                                if (stepCallback) {
                                    stepCallback({
                                        step: stepCount,
                                        answer: pl.format_answer(answer),
                                        variables: this.extractVariables(answer),
                                        type: 'success'
                                    });
                                }

                                results.push(result);
                                setTimeout(findSolutions, 100);
                            },
                            fail: () => {
                                if (stepCallback) {
                                    stepCallback({
                                        step: stepCount,
                                        type: 'complete',
                                        results: results
                                    });
                                }
                                resolve(results);
                            },
                            error: (err) => {
                                if (stepCallback) {
                                    stepCallback({
                                        step: stepCount,
                                        type: 'error',
                                        error: err.toString()
                                    });
                                }
                                reject(new Error(err.toString()));
                            }
                        });
                    };
                    findSolutions();
                },
                error: (err) => {
                    reject(new Error(`Error en consulta: ${err}`));
                }
            });
        });
    }

    extractVariables(answer) {
        const vars = {};
        if (answer && typeof answer === 'object') {
            for (let key in answer) {
                if (key.startsWith('_')) {
                    vars[key] = this.formatTerm(answer[key]);
                }
            }
        }
        return vars;
    }

    formatTerm(term) {
        if (typeof term === 'string') return term;
        if (typeof term === 'number') return term.toString();
        if (term && term.id === 'compound' && term.args) {
            return `${term.functor}(${term.args.map(arg => this.formatTerm(arg)).join(', ')})`;
        }
        return term ? term.toString() : 'null';
    }

    getProgramStructure() {
        return {
            facts: this.facts,
            rules: this.rules,
            predicates: Array.from(this.predicates.entries()),
            totalElements: this.facts.length + this.rules.length
        };
    }

    generateExampleProgram() {
        return `% Sistema experto para clasificación de hongos - VERSION CORREGIDA

% Reglas principales (en una sola línea cada una)
es_ingerible(Sombrero, Olor, Habitat) :- olor_seguro(Olor), forma_segura(Sombrero), habitat_seguro(Habitat).
es_venenoso(Sombrero, Olor, Habitat) :- olor_peligroso(Olor).

% Hechos de características
olor_seguro(almendra).
olor_seguro(anis).
olor_seguro(suave).
olor_peligroso(mohoso).
olor_peligroso(acre).
forma_segura(abultada).
forma_segura(chata).
forma_segura(abotonada).
forma_peligrosa(conica).
forma_peligrosa(irregular).
habitat_seguro(cercano).
habitat_seguro(bosque).
habitat_seguro(hojas).
habitat_peligroso(poblado).
habitat_peligroso(praderas).

% Regla de clasificación
clasificar_hongo(Sombrero, Olor, Habitat, ingerible) :- es_ingerible(Sombrero, Olor, Habitat).
clasificar_hongo(Sombrero, Olor, Habitat, venenoso) :- es_venenoso(Sombrero, Olor, Habitat).`;
    }

    printPredicateIndex() {
        console.log('=== ÍNDICE DE PREDICADOS ===');
        for (let [key, value] of this.predicateIndex) {
            console.log(`${key}:`, {
                rules: value.rules.length,
                facts: value.facts.length
            });
        }
    }
}

if (typeof window !== 'undefined') {
    window.PrologEngine = PrologEngine;
}