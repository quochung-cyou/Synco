import logging
import time
import asyncio
import httpx
from app.core.config import settings

logger = logging.getLogger(__name__)

class AIClient:
    def __init__(self):
        self.ibm_api_key = settings.IBM_CLOUD_API_KEY
        self.iam_url = settings.IBM_IAM_URL
        self.inference_url = settings.IBM_INFERENCE_URL
        
        self._access_token = None
        self._token_expires_at = 0
        self._lock = asyncio.Lock()

    async def _ensure_token(self):
        if self._access_token and time.time() < self._token_expires_at - 60:
             return

        async with self._lock:
            if self._access_token and time.time() < self._token_expires_at - 60:
                return

            logger.info("Access token missing or expired. Fetching new IBM IAM token...")
            
            async with httpx.AsyncClient() as client:
                headers = {
                    'Content-Type': 'application/x-www-form-urlencoded',
                    'Accept': 'application/json'
                }
                data = {
                    'grant_type': 'urn:ibm:params:oauth:grant-type:apikey',
                    'apikey': self.ibm_api_key
                }
                
                try:
                    response = await client.post(self.iam_url, headers=headers, data=data, timeout=10.0)
                    response.raise_for_status()
                    token_data = response.json()
                    
                    self._access_token = token_data['access_token']
                    self._token_expires_at = time.time() + token_data.get('expires_in', 3600)
                    logger.info(f"Successfully obtained IBM IAM token. Expires in {token_data.get('expires_in')} seconds.")
                    
                except httpx.HTTPError as e:
                    logger.error(f"Failed to obtain IBM IAM token: {e}")
                    if hasattr(e, 'response') and e.response:
                        logger.error(f"Response content: {e.response.text}")
                    raise

    async def call(self, prompt, system_prompt: str = "", config: dict = None) -> str:
        await self._ensure_token()
        
        async with httpx.AsyncClient() as client:
            headers = {
                'Content-Type': 'application/json',
                'Accept': 'application/json',
                'Authorization': f'Bearer {self._access_token}'
            }
            
            messages = []
            
            if isinstance(prompt, list):
                for msg in prompt:
                    role = msg.get("role", "user")
                    content = msg.get("content", "")
                    
                    if role == "system":
                        system_prompt = content
                        continue
                    
                    messages.append({
                        "role": role,
                        "content": [{"type": "text", "text": content}]
                    })
            else:
                messages.append({
                    "role": "user", 
                    "content": [{"type": "text", "text": str(prompt)}]
                })
            
            payload = {
                "messages": messages
            }
            
            if system_prompt:
                payload["context"] = system_prompt
            
            if config:
                payload.update(config)

            logger.info(f"Calling IBM Inference API...")
            
            max_retries = 3
            backoff_factor = 2

            for attempt in range(max_retries):
                try:
                    response = await client.post(self.inference_url, headers=headers, json=payload, timeout=120.0)
                    response.raise_for_status()
                    result = response.json()
                    
                    if 'choices' in result and len(result['choices']) > 0:
                        return result['choices'][0]['message']['content']
                    else:
                        logger.error(f"Unexpected response format: {result}")
                        return str(result)
                        
                except httpx.HTTPError as e:
                    logger.warning(f"Inference call attempt {attempt + 1}/{max_retries} failed: {e}")
                    if attempt < max_retries - 1:
                        sleep_time = backoff_factor ** attempt
                        logger.info(f"Retrying in {sleep_time} seconds...")
                        await asyncio.sleep(sleep_time)
                    else:
                        logger.error(f"Inference call failed after {max_retries} attempts: {e.response.status_code if hasattr(e, 'response') and e.response else 'Unknown'}")
                        if hasattr(e, 'response') and e.response:
                            logger.error(f"Response content: {e.response.text}")
                        raise
