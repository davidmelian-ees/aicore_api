import xsenv from "@sap/xsenv";
import { XssecPassportStrategy, XsuaaService, SECURITY_CONTEXT } from "@sap/xssec";
import passport from "passport";

xsenv.loadEnv();

// Cargar credenciales del servicio UAA
const services = xsenv.getServices({ xsuaa: { name: "aicore-app-auth" } });
const credentials = services.xsuaa;

console.log("Credenciales cargadas:", credentials.clientid, credentials.xsappname, credentials.clientsecret);

// Crear servicio de autenticación
const authService = new XsuaaService(credentials);

// Configurar Passport con JWT Strategy
passport.use(new XssecPassportStrategy(authService, SECURITY_CONTEXT));

// Función para inicializar la autenticación en Express
export function initAuth(app) {
  app.use(passport.initialize());
  app.use(passport.authenticate("JWT", { session: false }));
}

