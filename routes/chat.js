import express from "express";
import { getAiCoreClient } from "../auth/aiCoreClient.js";

const router = express.Router();

// Endpoint para hacer consultas al modelo
router.post("/chat", async (req, res) => {
  try {
    const { message } = req.body;
    if (!message) {
      return res.status(400).json({ error: "Message is required" });
    }

    const client = getAiCoreClient();
    const response = await client.run({
      messages: [{ role: "user", content: message }]
    });

    res.json({
      reply: response.getContent()
    });
  } catch (err) {
    console.error("Error in /chat:", err);
    res.status(500).json({ error: "Internal Server Error" });
  }
});

export default router;
