const pdfjsLib = require('pdfjs-dist/legacy/build/pdf.js');
const fs = require('fs').promises;

class PDFVisualAnalyzer {
    
    /**
     * Analiza el interlineado de los párrafos del PDF
     */
    async analyzeLineSpacing(pdfPath) {
        try {
            const data = await fs.readFile(pdfPath);
            const pdf = await pdfjsLib.getDocument({ data }).promise;
            
            const inconsistencies = [];
            const spacings = [];
            
            for (let pageNum = 1; pageNum <= pdf.numPages; pageNum++) {
                const page = await pdf.getPage(pageNum);
                const textContent = await page.getTextContent();
                
                let previousY = null;
                let previousHeight = null;
                
                for (let item of textContent.items) {
                    const lineHeight = item.height;
                    const yPosition = item.transform[5];
                    
                    if (previousY && previousHeight) {
                        const spacing = Math.abs(previousY - yPosition);
                        const lineSpacing = spacing / previousHeight;
                        
                        // Solo considerar espaciados significativos (no dentro de la misma línea)
                        if (lineSpacing > 0.5 && lineSpacing < 5) {
                            spacings.push(lineSpacing);
                        }
                    }
                    
                    previousY = yPosition;
                    previousHeight = lineHeight;
                }
            }
            
            // Calcular espaciado estándar (moda)
            const standardSpacing = this.calculateMode(spacings);
            
            // Detectar inconsistencias (variación >20%)
            for (let pageNum = 1; pageNum <= pdf.numPages; pageNum++) {
                const page = await pdf.getPage(pageNum);
                const textContent = await page.getTextContent();
                
                let previousY = null;
                let previousHeight = null;
                let lineNum = 0;
                
                for (let item of textContent.items) {
                    const lineHeight = item.height;
                    const yPosition = item.transform[5];
                    
                    if (previousY && previousHeight) {
                        const spacing = Math.abs(previousY - yPosition);
                        const lineSpacing = spacing / previousHeight;
                        
                        if (lineSpacing > 0.5 && lineSpacing < 5) {
                            const variation = Math.abs(lineSpacing - standardSpacing) / standardSpacing;
                            
                            if (variation > 0.2) {
                                inconsistencies.push({
                                    page: pageNum,
                                    line: lineNum,
                                    spacing: lineSpacing.toFixed(2),
                                    expected: standardSpacing.toFixed(2),
                                    variation: (variation * 100).toFixed(1) + '%'
                                });
                            }
                            lineNum++;
                        }
                    }
                    
                    previousY = yPosition;
                    previousHeight = lineHeight;
                }
            }
            
            return {
                inconsistencies: inconsistencies.slice(0, 10), // Limitar a 10 para no saturar
                standardSpacing: standardSpacing.toFixed(2),
                totalInconsistencies: inconsistencies.length
            };
            
        } catch (error) {
            console.error('Error analizando interlineado:', error);
            return { inconsistencies: [], standardSpacing: '1.15', totalInconsistencies: 0 };
        }
    }
    
    /**
     * Analiza si las cabeceras de tablas tienen negrita
     */
    async analyzeTableHeaders(pdfPath) {
        try {
            const data = await fs.readFile(pdfPath);
            const pdf = await pdfjsLib.getDocument({ data }).promise;
            
            const tablesWithoutBold = [];
            let totalTables = 0;
            
            for (let pageNum = 1; pageNum <= pdf.numPages; pageNum++) {
                const page = await pdf.getPage(pageNum);
                const textContent = await page.getTextContent();
                
                // Detectar posibles cabeceras de tabla (texto en mayúsculas o con pipes)
                const items = textContent.items;
                
                for (let i = 0; i < items.length; i++) {
                    const item = items[i];
                    const text = item.str.trim();
                    
                    // Detectar posible cabecera (mayúsculas, corto, seguido de más texto)
                    if (text.length > 2 && text.length < 30 && 
                        (text === text.toUpperCase() || text.includes('|'))) {
                        
                        const fontName = item.fontName || '';
                        const isBold = fontName.toLowerCase().includes('bold');
                        
                        if (!isBold) {
                            // Verificar si hay más texto similar en la misma línea (indica tabla)
                            const sameLineItems = items.filter(it => 
                                Math.abs(it.transform[5] - item.transform[5]) < 2
                            );
                            
                            if (sameLineItems.length >= 2) {
                                totalTables++;
                                tablesWithoutBold.push({
                                    page: pageNum,
                                    header: text,
                                    font: fontName,
                                    position: `Y: ${item.transform[5].toFixed(0)}`
                                });
                            }
                        }
                    }
                }
            }
            
            return {
                tablesWithoutBold: tablesWithoutBold.slice(0, 10), // Limitar a 10
                totalTables: totalTables,
                totalWithoutBold: tablesWithoutBold.length
            };
            
        } catch (error) {
            console.error('Error analizando cabeceras:', error);
            return { tablesWithoutBold: [], totalTables: 0, totalWithoutBold: 0 };
        }
    }
    
    /**
     * Analiza si las tablas ocupan el ancho completo de la página
     */
    async analyzeTableWidths(pdfPath) {
        try {
            const data = await fs.readFile(pdfPath);
            const pdf = await pdfjsLib.getDocument({ data }).promise;
            
            const narrowTables = [];
            
            for (let pageNum = 1; pageNum <= pdf.numPages; pageNum++) {
                const page = await pdf.getPage(pageNum);
                const viewport = page.getViewport({ scale: 1.0 });
                const pageWidth = viewport.width;
                
                // Márgenes estándar (72pt = 1 pulgada)
                const leftMargin = 72;
                const rightMargin = 72;
                const usableWidth = pageWidth - leftMargin - rightMargin;
                
                const textContent = await page.getTextContent();
                const items = textContent.items;
                
                // Agrupar items por línea (mismo Y)
                const lines = {};
                items.forEach(item => {
                    const y = Math.round(item.transform[5]);
                    if (!lines[y]) lines[y] = [];
                    lines[y].push(item);
                });
                
                // Detectar líneas con múltiples columnas (posibles tablas)
                Object.entries(lines).forEach(([y, lineItems]) => {
                    if (lineItems.length >= 2) {
                        const minX = Math.min(...lineItems.map(i => i.transform[4]));
                        const maxX = Math.max(...lineItems.map(i => i.transform[4] + i.width));
                        const tableWidth = maxX - minX;
                        
                        const widthPercentage = (tableWidth / usableWidth) * 100;
                        
                        // Si ocupa menos del 90% del ancho útil
                        if (widthPercentage < 90 && lineItems.length >= 2) {
                            const text = lineItems.map(i => i.str).join(' | ').substring(0, 50);
                            narrowTables.push({
                                page: pageNum,
                                text: text,
                                widthPercentage: widthPercentage.toFixed(1) + '%',
                                actualWidth: tableWidth.toFixed(0) + 'pt',
                                expectedWidth: usableWidth.toFixed(0) + 'pt'
                            });
                        }
                    }
                });
            }
            
            return {
                narrowTables: narrowTables.slice(0, 10), // Limitar a 10
                pageWidth: 595, // A4 estándar
                usableWidth: 451,
                totalNarrowTables: narrowTables.length
            };
            
        } catch (error) {
            console.error('Error analizando anchos de tabla:', error);
            return { narrowTables: [], pageWidth: 595, usableWidth: 451, totalNarrowTables: 0 };
        }
    }
    
    /**
     * Ejecuta todos los análisis visuales
     */
    async analyzeAll(pdfPath) {
        try {
            const [spacing, headers, widths] = await Promise.all([
                this.analyzeLineSpacing(pdfPath),
                this.analyzeTableHeaders(pdfPath),
                this.analyzeTableWidths(pdfPath)
            ]);
            
            return {
                lineSpacing: spacing,
                tableHeaders: headers,
                tableWidths: widths
            };
        } catch (error) {
            console.error('Error en análisis visual completo:', error);
            return {
                lineSpacing: { inconsistencies: [], standardSpacing: '1.15', totalInconsistencies: 0 },
                tableHeaders: { tablesWithoutBold: [], totalTables: 0, totalWithoutBold: 0 },
                tableWidths: { narrowTables: [], pageWidth: 595, usableWidth: 451, totalNarrowTables: 0 }
            };
        }
    }
    
    /**
     * Calcula la moda (valor más frecuente) de un array
     */
    calculateMode(arr) {
        if (arr.length === 0) return 1.15;
        
        const rounded = arr.map(v => Math.round(v * 10) / 10);
        const frequency = {};
        let maxFreq = 0;
        let mode = 1.15;
        
        rounded.forEach(val => {
            frequency[val] = (frequency[val] || 0) + 1;
            if (frequency[val] > maxFreq) {
                maxFreq = frequency[val];
                mode = val;
            }
        });
        
        return mode;
    }
    
    /**
     * Genera reporte de texto de errores visuales
     */
    generateVisualErrorsReport(visualAnalysis) {
        let report = '';
        
        // Interlineado
        if (visualAnalysis.lineSpacing.totalInconsistencies > 0) {
            report += '\n🟡 ADVERTENCIA: Interlineado inconsistente detectado\n\n';
            report += `Interlineado estándar: ${visualAnalysis.lineSpacing.standardSpacing}\n`;
            report += `Total inconsistencias: ${visualAnalysis.lineSpacing.totalInconsistencies}\n\n`;
            
            visualAnalysis.lineSpacing.inconsistencies.forEach(inc => {
                report += `• Página ${inc.page}: Interlineado ${inc.spacing} (esperado ${inc.expected}, variación ${inc.variation})\n`;
            });
            
            if (visualAnalysis.lineSpacing.totalInconsistencies > 10) {
                report += `\n... y ${visualAnalysis.lineSpacing.totalInconsistencies - 10} inconsistencias más\n`;
            }
        }
        
        // Cabeceras sin negrita
        if (visualAnalysis.tableHeaders.totalWithoutBold > 0) {
            report += '\n🟡 ADVERTENCIA: Cabeceras de tabla sin negrita\n\n';
            report += `Total tablas sin negrita: ${visualAnalysis.tableHeaders.totalWithoutBold}\n\n`;
            
            visualAnalysis.tableHeaders.tablesWithoutBold.forEach(table => {
                report += `• Página ${table.page}: "${table.header}" (fuente: ${table.font})\n`;
            });
            
            if (visualAnalysis.tableHeaders.totalWithoutBold > 10) {
                report += `\n... y ${visualAnalysis.tableHeaders.totalWithoutBold - 10} tablas más\n`;
            }
        }
        
        // Tablas reducidas
        if (visualAnalysis.tableWidths.totalNarrowTables > 0) {
            report += '\n🟡 ADVERTENCIA: Tablas reducidas detectadas\n\n';
            report += `Total tablas reducidas: ${visualAnalysis.tableWidths.totalNarrowTables}\n\n`;
            
            visualAnalysis.tableWidths.narrowTables.forEach(table => {
                report += `• Página ${table.page}: "${table.text}..." ocupa ${table.widthPercentage} (esperado >90%)\n`;
            });
            
            if (visualAnalysis.tableWidths.totalNarrowTables > 10) {
                report += `\n... y ${visualAnalysis.tableWidths.totalNarrowTables - 10} tablas más\n`;
            }
        }
        
        return report;
    }
}

export default new PDFVisualAnalyzer();
