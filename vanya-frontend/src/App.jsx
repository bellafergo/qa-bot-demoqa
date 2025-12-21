import React, { useState, useEffect, useRef } from 'react';
import './App.css';

function App() {
  const [messages, setMessages] = useState([]);
  const [input, setInput] = useState('');
  const [isLoading, setIsLoading] = useState(false);
  const chatEndRef = useRef(null);

  // URL de tu API en Render (Verificada)
  const API_BASE = "https://qa-bot-demoqa.onrender.com"; 

  const scrollToBottom = () => {
    chatEndRef.current?.scrollIntoView({ behavior: "smooth" });
  };

  useEffect(() => {
    // Saludo inicial de Vanya
    setMessages([{
      role: 'bot',
      content: 'Hola, soy **Vanya**, tu Agente de QA inteligente. ¿En qué puedo ayudarte hoy con tus pruebas?'
    }]);
  }, []);

  useEffect(scrollToBottom, [messages]);

  // Función para formatear el texto de Markdown a HTML básico
  const formatText = (text) => {
    if (!text) return "";
    return text
      .replace(/\*\*(.*?)\*\*/g, '<b>$1</b>') // Negritas
      .replace(/\n/g, '<br/>');               // Saltos de línea
  };

  const handleSend = async () => {
    if (!input.trim() || isLoading) return;

    const userMsg = { role: 'user', content: input };
    setMessages(prev => [...prev, userMsg]);
    
    const currentInput = input;
    setInput('');
    setIsLoading(true);

    try {
      const resp = await fetch(`${API_BASE}/ask`, {
        method: 'POST',
        headers: { 'Content-Type': 'application/json' },
        body: JSON.stringify({ 
          prompt: currentInput, 
          session_id: "anon" 
        })
      });
      
      if (!resp.ok) throw new Error(`Error server: ${resp.status}`);
      
      const data = await resp.json();
      
      setMessages(prev => [...prev, {
        role: 'bot',
        content: data.answer || "Ejecución finalizada.",
        runner: data.runner || data.run_result // Compatible con ambas estructuras
      }]);
    } catch (error) {
      console.error("Error en la petición:", error);
      setMessages(prev => [...prev, { 
        role: 'bot', 
        content: "❌ **Error de conexión con Vanya.**\n\nEl servidor en Render podría estar tardando en despertar o la URL es incorrecta. Por favor, verifica los logs en Render." 
      }]);
    } finally {
      setIsLoading(false);
    }
  };

  return (
    <div className="vanya-wrap">
      <header className="vanya-header">
        <div className="logo-dot"></div>
        <h1>Vanya <small>| QA Intelligence Agent</small></h1>
      </header>

      <main className="chat-area">
        {messages.map((msg, i) => (
          <div key={i} className={`message-row ${msg.role}`}>
            <div className="message-label">{msg.role === 'user' ? 'Tú' : 'Vanya'}</div>
            <div className="bubble">
              <div 
                className="text-content" 
                dangerouslySetInnerHTML={{ __html: formatText(msg.content) }} 
              />
              
              {/* Renderizado de Evidencia (Screenshot) */}
              {msg.runner?.screenshot_b64 && (
                <div className="evidence-container">
                  <p className="ev-title">🖼️ Evidencia de ejecución:</p>
                  <img 
                    src={`data:image/png;base64,${msg.runner.screenshot_b64}`} 
                    alt="Evidencia" 
                    className="evidence-img"
                    onClick={() => {
                      const newTab = window.open();
                      newTab.document.write(`<img src="data:image/png;base64,${msg.runner.screenshot_b64}" style="width:100%">`);
                    }}
                  />
                  <p style={{fontSize: '10px', marginTop: '5px', opacity: 0.5}}>Click para ampliar</p>
                </div>
              )}
            </div>
          </div>
        ))}
        
        {isLoading && (
          <div className="message-row bot">
            <div className="message-label">Vanya</div>
            <div className="bubble loading">Vanya está procesando tu solicitud...</div>
          </div>
        )}
        <div ref={chatEndRef} />
      </main>

      <footer className="input-area">
        <textarea 
          value={input}
          onChange={(e) => setInput(e.target.value)}
          placeholder="Escribe una orden de QA o pregunta algo..."
          disabled={isLoading}
          onKeyDown={(e) => {
            if (e.key === 'Enter' && !e.shiftKey) {
              e.preventDefault();
              handleSend();
            }
          }}
        />
        <button onClick={handleSend} disabled={isLoading || !input.trim()}>
          {isLoading ? '...' : 'Enviar'}
        </button>
      </footer>
    </div>
  );
}

export default App;