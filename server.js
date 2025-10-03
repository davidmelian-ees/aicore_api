import express from "express";
import { initAuth } from "./auth.js";
import chatRoutes from "./routes/chat.js";
const app = express();

// Inicializar autenticación
//initAuth(app);
app.use(express.json());

// Rutas
app.use("/api", chatRoutes);

const port = process.env.PORT || 4000;
app.listen(port, () => {
  console.log(`🚀 Server running at http://localhost:${port}`);
});