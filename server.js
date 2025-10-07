import express from "express";
import { initAuth } from "./auth.js";
import chatRoutes from "./routes/chat.js";
import testChromadbConRoutes from "./routes/testChromadbCon.js"
import createCollectionsChromaRoutes from "./routes/createCollectionChroma.js"
import getCollectionsChromaRoutes from "./routes/getCollectionsChroma.js"
const app = express();

// Inicializar autenticación
//initAuth(app);
app.use(express.json());


// Rutas
app.use("/api", chatRoutes);
app.use("/api", testChromadbConRoutes)
app.use("/api", createCollectionsChromaRoutes)
app.use("/api", getCollectionsChromaRoutes)

const port = process.env.PORT || 4000;
app.listen(port, () => {
  console.log(`🚀 Server running at http://localhost:${port}`);
});