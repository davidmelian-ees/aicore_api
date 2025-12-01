import xsenv from "@sap/xsenv";
import { AzureOpenAiChatClient } from "@sap-ai-sdk/foundation-models";

xsenv.loadEnv(); // Carga default-env.json si existe

const services = xsenv.getServices({
  aicore: { name: "default_aicore" },
  xsuaa: { name: "aicore-app-auth" }
});
console.log("MYSERVICE", services)

console.log("AI Core credentials:", services.aicore);

// el SDK usa automáticamente el binding de aicore
const client = new AzureOpenAiChatClient("gpt-4o-mini");

const response = await client.run({
  messages: [{ role: "user", content: "Hello from AI Core!" }]
});

console.log("Response:", response.getContent());