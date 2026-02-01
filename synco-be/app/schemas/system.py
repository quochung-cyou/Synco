from pydantic import BaseModel

class SystemStatus(BaseModel):
    status: str
    version: str
    message: str
