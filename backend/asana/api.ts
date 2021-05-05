import fetch from "isomorphic-fetch";
import { token } from "../config/asana";

export class HttpError extends Error {
  status: number;
  errorMessage: string;
  __isHttpError: boolean;

  constructor(status: number, message: string) {
    super(`HTTP ${status}: ${message}`);
    this.status = status;
    this.errorMessage = message;
    this.__isHttpError = true;
  }
}

export interface AsanaTask {
  gid: string;
  resource_type: "task";
  name: string;
}

export type AsanaEntity = AsanaTask;

export interface AsanaResponse {
  data: AsanaEntity[] | AsanaEntity;
}


export async function getFromAsana(path: string): Promise<AsanaResponse|null> {
  const url = `https://app.asana.com/api/1.0/${path}`;
  const headers = {
    "Accept": "application/json",
    "Authorization": `Bearer ${token}`,
  };
  const response = await fetch(url, { headers });
  switch(response.status) {
    case 200:
      return response.json();
    case 204:
      return null;
    default:
      throw new HttpError(response.status, `unable to retrieve ${path} from Asana`);
  }
}

export async function getTasksForProject(projectId: string): Promise<AsanaTask[]> {
  const { data } =
    await getFromAsana(`projects/${projectId}/tasks`);
  return data as AsanaTask[];
}
