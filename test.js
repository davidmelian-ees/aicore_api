import xsenv from "@sap/xsenv";
import { AzureOpenAiChatClient } from "@sap-ai-sdk/foundation-models";

xsenv.loadEnv();

const client = new AzureOpenAiChatClient('gpt-4o');
const response = await client.run({
  messages: [
    {
      role: 'user',
      content: 'Where is the deepest place on earth located?'
    }
  ]
});
console.log(response.getContent());