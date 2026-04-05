<!-- trigger vanya -->
## Vanya v0.3 — QA Reasoning Engine

### 🚀 New Features
- Introduced Suggested Tests Engine
- Generates QA test cases automatically from DOM inventory
- Supports:
  - Form validation tests
  - Required field negative tests
  - Search behavior tests
  - Navigation smoke tests

### 🧠 Improvements
- Deterministic test generation (no hallucinations)
- Priority-based test ordering (high → medium → low)
- Deduplication of generated test cases

### 🧪 Testing
- Added 44 unit tests for suggested_tests module
- Total test suite: 276 passing

### 🏗️ Architecture
Application Explorer → DOM Inventory → Suggested Tests → (Execution next)

### 🎯 Impact
Vanya evolves from test executor → QA assistant capable of reasoning about what to test