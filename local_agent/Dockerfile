# Minimal Vanya Local Agent image (Phase 4B — httpx only, no Playwright).
FROM python:3.12-slim-bookworm

WORKDIR /app

RUN pip install --no-cache-dir "httpx>=0.26.0,<0.28"

# Build context should be this directory (local_agent/).
COPY . /app/local_agent

ENV PYTHONPATH=/app

ENTRYPOINT ["python", "-m", "local_agent.cli"]
