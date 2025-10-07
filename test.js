import { createCollection, listCollections, addDocument } from "./Utils/chromadb.js";

async function main() {
  // 1. Crear colección (si no existe ya)
  /*await createCollection("demodos", 3);

  // 2. Listar colecciones
  const collections = await listCollections();

  // 3. Buscar la colección que acabamos de crear
  const myCol = collections?.collections?.find(c => c.name === "demodos");
  if (!myCol) {
    console.error("❌ No se encontró la colección 'demodos'");
    return;
  }*/

  console.log("➡️ Usando colección:", "7e15392b-b107-4d0c-95d0-b971040f914e");

  // 4. Insertar documento
  await addDocument("7e15392b-b107-4d0c-95d0-b971040f914e");
}

main();