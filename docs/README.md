# Node.js + XSUAA Authentication + express

Este proyecto es un ejemplo de **API Node.js protegida con XSUAA** en SAP BTP, usando `@sap/xssec` y `passport` para la autenticación JWT.

---

## Requisitos

- Node.js **v20.x** (recomendado)
- npm
- SAP BTP subaccount con **XSUAA** service provisionado
- Cloud Foundry CLI (`cf`) si quieres desplegar
- Postman o `curl` para probar la API

---
## Configuración
1. hacer "cf login -sso" y autenticar
2. Hacer bind de la app actual con la autenticación "cf bind-service nombre-mi-app my-auth"
    2.1. Importante haber configurado la autenticación previamente con el xs-security.json 
    2.2. Si no tienes la autenticación creada crearla en base a nuestro xs-security.json "cf create-service xsuaa application aicore-app-auth -c xs-security.json"
3. Cambiar nombre app en el package y en el manifest.yml

## Instalación
1. Clonar el repositorio:

```bash
git clone https://github.com/davidmelian-ees/template-api-oauth-cap-service-vanture

cd repo
npm i
touch default-env.json #con las credenciales proporcionadas
```
2. Cambiar en el package.json y en el manifest.yml el nombre de la app de autenticación 


