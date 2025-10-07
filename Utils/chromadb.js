import axios from "axios";

const BASE_URL = "https://chromadb-server.cfapps.eu10-005.hana.ondemand.com/api/v2";
const TENANT = "demo";
const DATABASE = "demodb";

// 👉 cambia el nombre si quieres usar otro
const COLLECTION_NAME = "democollection";

// 🔹 Si necesitas autenticación, añade tu token aquí
const HEADERS = {
  "Content-Type": "application/json",
  // "Authorization": `Bearer ${process.env.CHROMA_TOKEN}`
};

// Crear colección con dimensión fija
async function createCollection(name = COLLECTION_NAME, dimension = 3) {
  try {
    const res = await axios.post(
      `${BASE_URL}/tenants/${TENANT}/databases/${DATABASE}/collections`,
      { name, dimension },
      { headers: HEADERS }
    );
    console.log("✅ Colección creada:", res.data);
    return res.data;
  } catch (err) {
    console.error("⚠️ Error creando colección:", err.response?.data || err.message);
  }
}

// Listar colecciones
async function listCollections() {
  try {
    const res = await axios.get(
      `${BASE_URL}/tenants/${TENANT}/databases/${DATABASE}/collections`,
      { headers: HEADERS }
    );
    console.log("📂 Colecciones:", res.data);
    return res.data;
  } catch (err) {
    console.error("⚠️ Error listando colecciones:", err.response?.data || err.message);
  }
}

// Insertar documento en una colección
async function addDocument(collectionId, documents, ids, embeddings) {
  try {
    const body = {
      documents: documents,
      embeddings: [embeddings], // dimensión = 3
      ids: ids,
      metadatas: [{ source: "nodejs-demo" }]
    };

    const res = await axios.post(
      `${BASE_URL}/tenants/${TENANT}/databases/${DATABASE}/collections/${collectionId}/add`,
      body,
      { headers: HEADERS }
    );

    console.log("✅ Documento insertado:", res.data);
    return res.data;
  } catch (err) {
    console.error("⚠️ Error insertando documento:", err.response?.data || err.message);
  }
}

// Exportar funciones
export { createCollection, listCollections, addDocument };