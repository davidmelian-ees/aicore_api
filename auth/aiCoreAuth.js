import xsenv from "@sap/xsenv"

xsenv.loadEnv(); // Carga default-env.json si existe

export function initializeServices(){
    const services = xsenv.getServices({
        aicore: { name: "default_aicore" },
        xsuaa: { name: "aicore-app-auth" }
    });
    console.log("AI Core service initialized:");
}

