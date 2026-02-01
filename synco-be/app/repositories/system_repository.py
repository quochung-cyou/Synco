import platform

class SystemRepository:
    def get_system_info(self) -> dict:
        return {
            "status": "online",
            "version": "1.0.0",
            "message": f"Running on {platform.system()} {platform.release()}"
        }
