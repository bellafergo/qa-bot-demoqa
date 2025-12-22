# Imagen oficial de Playwright (YA TRAE CHROMIUM)
FROM mcr.microsoft.com/playwright/python:v1.49.0-jammy

# Variables Python
ENV PYTHONDONTWRITEBYTECODE=1
ENV PYTHONUNBUFFERED=1

# Directorio de trabajo
WORKDIR /app

# Copiar dependencias
COPY requirements.txt .

RUN pip install --upgrade pip && pip install -r requirements.txt

# Copiar el código
COPY . .

# Puerto FastAPI
EXPOSE 8000

# Arranque
CMD ["uvicorn", "app:app", "--host", "0.0.0.0", "--port", "8000"]