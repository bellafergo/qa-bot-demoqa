# Usa la imagen oficial de Playwright para Python (ya incluye Chromium y dependencias)
FROM mcr.microsoft.com/playwright/python:v1.49.0-jammy

# Directorio de trabajo dentro del contenedor
WORKDIR /app

# Copiamos solo requirements primero para aprovechar cache
COPY requirements.txt .

# Instalamos dependencias de Python (incluye playwright en tu requirements)
RUN pip install --no-cache-dir -r requirements.txt

# Copiamos el resto del proyecto
COPY . .

# Render expone el puerto vía la variable de entorno PORT
ENV PORT=10000

# Comando de arranque – usa el PORT que le da Render
CMD ["bash", "-c", "uvicorn app:app --host 0.0.0.0 --port ${PORT}"]
